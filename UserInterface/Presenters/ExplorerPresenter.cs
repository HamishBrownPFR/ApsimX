﻿// -----------------------------------------------------------------------
// <copyright file="ExplorerPresenter.cs"  company="APSIM Initiative">
//     Copyright (c) APSIM Initiative
// </copyright>
// -----------------------------------------------------------------------
namespace UserInterface.Presenters
{
    using System;
    using System.Collections.Generic;
    using System.Data;
    using System.IO;
    using System.Reflection;
    using System.Runtime.Serialization;
    using System.Windows.Forms;
    using Commands;
    using EventArguments;
    using Interfaces;
    using Models;
    using Models.Core;
    using Views;
    using APSIM.Shared.Utilities;

    /// <summary>
    /// This presenter class is responsible for populating the view
    /// passed into the constructor and handling all user interaction of 
    /// the view. Humble Dialog pattern.
    /// </summary>
    public class ExplorerPresenter : IPresenter
    {
        /// <summary>
        /// The visual instance
        /// </summary>
        private IExplorerView view;

        /// <summary>
        /// The main menu
        /// </summary>
        private MainMenu mainMenu;

        /// <summary>
        /// The context menu
        /// </summary>
        private ContextMenu contextMenu;

        /// <summary>
        /// Presenter for the component
        /// </summary>
        private IPresenter currentRightHandPresenter;

        /// <summary>
        /// Using advanced mode
        /// </summary>
        private bool advancedMode = false;

        /// <summary>
        /// Initializes a new instance of the <see cref="ExplorerPresenter" /> class.
        /// </summary>
        public ExplorerPresenter()
        {
        }

        /// <summary>
        /// Gets or sets the command history for this presenter       
        /// </summary>
        public CommandHistory CommandHistory { get; set; }
        
        /// <summary>
        /// Gets or sets the APSIMX simulations object
        /// </summary>
        public Simulations ApsimXFile { get; set; }
        
        /// <summary>
        /// Gets or sets the width of the explorer tree panel
        /// </summary>
        public int TreeWidth
        {
            get { return this.view.TreeWidth; }
            set { this.view.TreeWidth = value; }
        }

        /// <summary>
        /// Gets the current right hand presenter.
        /// </summary>
        public IPresenter CurrentPresenter
        {
            get
            {
                return this.currentRightHandPresenter;
            }
        }

        /// <summary>
        /// Gets the path of the current selected node in the tree.
        /// </summary>
        public string CurrentNodePath
        {
            get
            {
                return this.view.CurrentNodePath;
            }
        }
        
        /// <summary>
        /// Attach the view to this presenter and begin populating the view.
        /// </summary>
        /// <param name="model">The simulation model</param>
        /// <param name="view">The view used for display</param>
        /// <param name="explorerPresenter">The presenter for this object</param>
        public void Attach(object model, object view, ExplorerPresenter explorerPresenter)
        {
            this.CommandHistory = new CommandHistory();
            this.ApsimXFile = model as Simulations;
            this.view = view as IExplorerView;
            this.mainMenu = new MainMenu(this);
            this.contextMenu = new ContextMenu(this);

            this.view.ShortcutKeys = new Keys[] { Keys.F5 };
            this.view.PopulateChildNodes += this.OnPopulateNodes;
            this.view.PopulateContextMenu += this.OnPopulateContextMenu;
            this.view.PopulateMainMenu += this.OnPopulateMainMenu;
            this.view.NodeSelectedByUser += this.OnNodeSelectedByUser;
            this.view.NodeSelected += this.OnNodeSelected;
            this.view.DragStart += this.OnDragStart;
            this.view.AllowDrop += this.OnAllowDrop;
            this.view.Drop += this.OnDrop;
            this.view.Rename += this.OnRename;
            this.view.OnMoveDown += this.OnMoveDown;
            this.view.OnMoveUp += this.OnMoveUp;
            this.view.OnShortcutKeyPress += this.OnShortcutKeyPress;

            this.CommandHistory.ModelStructureChanged += this.OnModelStructureChanged;

            this.WriteLoadErrors();
        }

        /// <summary>
        /// Detach the model from the view.
        /// </summary>
        public void Detach()
        {
            this.view.PopulateChildNodes -= this.OnPopulateNodes;
            this.view.PopulateContextMenu -= this.OnPopulateContextMenu;
            this.view.PopulateMainMenu -= this.OnPopulateMainMenu;
            this.view.NodeSelectedByUser -= this.OnNodeSelectedByUser;
            this.view.NodeSelected -= this.OnNodeSelected;
            this.view.DragStart -= this.OnDragStart;
            this.view.AllowDrop -= this.OnAllowDrop;
            this.view.Drop -= this.OnDrop;
            this.view.Rename -= this.OnRename;
            this.view.OnMoveDown -= this.OnMoveDown;
            this.view.OnMoveUp -= this.OnMoveUp;
            this.view.OnShortcutKeyPress -= this.OnShortcutKeyPress;

            this.CommandHistory.ModelStructureChanged -= this.OnModelStructureChanged;
        }

        /// <summary>
        /// Toggle advanced mode.
        /// </summary>
        public void ToggleAdvancedMode()
        {
            this.advancedMode = !this.advancedMode;
            this.view.InvalidateNode(".Simulations", this.GetNodeDescription(this.ApsimXFile));
        }

        /// <summary>
        /// Called by TabbedExplorerPresenter to do a save. Return true if all ok.
        /// </summary>
        /// <returns>True if saved</returns>
        public bool SaveIfChanged()
        {
            bool result = true;
            try
            {
                if (this.ApsimXFile != null && this.ApsimXFile.FileName != null)
                {
                    // need to test is ApsimXFile has changed and only prompt when changes have occured.
                    // serialise ApsimXFile to buffer
                    StringWriter o = new StringWriter();
                    this.ApsimXFile.Write(o);
                    string newSim = o.ToString();

                    StreamReader simStream = new StreamReader(this.ApsimXFile.FileName);
                    string origSim = simStream.ReadToEnd(); // read original file to buffer2
                    simStream.Close();

                    int choice = 1;                           // no save
                    if (string.Compare(newSim, origSim) != 0)   
                    {
                        choice = this.view.AskToSave();
                    }

                    if (choice == -1)
                    {   // cancel
                        result = false;
                    }
                    else if (choice == 0)
                    {
                        // save
                        // Need to hide the right hand panel because some views may not have saved
                        // their contents until they get a 'Detach' call.
                        this.HideRightHandPanel();

                        this.WriteSimulation();
                        result = true;
                    }
                }
            }
            catch (Exception err)
            {
                this.view.ShowMessage("Cannot save the file. Error: " + err.Message, DataStore.ErrorLevel.Error);
                result = false;
            }

            return result;
        }

        /// <summary>
        /// Save all changes.
        /// </summary>
        /// <returns>True if file was saved.</returns>
        public bool Save()
        {
            try
            {
                // Need to hide the right hand panel because some views may not have saved
                // their contents until they get a 'Detach' call.
                this.HideRightHandPanel();
                
                if (this.ApsimXFile.FileName == null)
                    this.SaveAs();

                if (this.ApsimXFile.FileName != null)
                {
                    this.ApsimXFile.Write(this.ApsimXFile.FileName);
                    return true;
                }
            }
            catch (Exception err)
            {
                this.ShowMessage("Cannot save the file. Error: " + err.Message, DataStore.ErrorLevel.Error);
            }

            return false;
        }

        /// <summary>
        /// Save the current file under a different name.
        /// </summary>
        /// <returns>True if file was saved.</returns>
        public bool SaveAs()
        {
            string newFileName = this.view.SaveAs(this.ApsimXFile.FileName);
            if (newFileName != null)
            {
                try
                {
                    if (this.ApsimXFile.FileName != null)
                        Utility.Configuration.Settings.DelMruFile(this.ApsimXFile.FileName);

                    Utility.Configuration.Settings.AddMruFile(newFileName);
                    this.ApsimXFile.Write(newFileName);
                    this.view.ChangeTabText(Path.GetFileNameWithoutExtension(newFileName));
                    return true;
                }
                catch (Exception err)
                {
                    this.ShowMessage("Cannot save the file. Error: " + err.Message, DataStore.ErrorLevel.Error);
                }
            }

            return false;
        }
 
        /// <summary>
        /// Toggle the second right hand side explorer view on/off
        /// </summary>
        public void ToggleSecondExplorerViewVisible()
        {
            this.view.ToggleSecondExplorerViewVisible();
        }

        /// <summary>
        /// Do the actual write to the file
        /// </summary>
        public void WriteSimulation()
        {
            this.ApsimXFile.ExplorerWidth = this.TreeWidth;
            this.ApsimXFile.Write(this.ApsimXFile.FileName);
        }

        /// <summary>
        /// Add a status message to the explorer window
        /// </summary>
        /// <param name="message">Status message</param>
        /// <param name="errorLevel">Level for the error message</param>
        public void ShowMessage(string message, Models.DataStore.ErrorLevel errorLevel)
        {
            this.view.ShowMessage(message, errorLevel);
        }
                
        /// <summary>
        /// A helper function that asks user for a folder.
        /// </summary>
        /// <param name="prompt">Prompt string</param>
        /// <returns>Returns the selected folder or null if action cancelled by user.</returns>
        public string AskUserForFolder(string prompt)
        {
            return this.view.AskUserForFolder(prompt);
        }

        /// <summary>
        /// Select a node in the view.
        /// </summary>
        /// <param name="nodePath">Path to node</param>
        public void SelectNode(string nodePath)
        {
            this.view.CurrentNodePath = nodePath;
        }

        /// <summary>
        /// Select the next node in the view. The next node is defined as the next one
        /// down in the tree view. It will go through child nodes if they exist.
        /// Will return true if next node was successfully selected. Will return
        /// false if no more nodes to select.
        /// </summary>
        /// <returns>True when node is selected</returns>
        public bool SelectNextNode()
        {
            this.HideRightHandPanel();

            // Get a complete list of all models in this file.
            List<IModel> allModels = Apsim.ChildrenRecursivelyVisible(this.ApsimXFile);

            /* If the current node path is '.Simulations' (the root node) then
               select the first item in the 'allModels' list. */
            if (this.view.CurrentNodePath == ".Standard toolbox")
            {
                this.view.CurrentNodePath = Apsim.FullPath(allModels[0]);
                return true;
            }

            // Find the current node in this list.
            int index = -1;
            for (int i = 0; i < allModels.Count; i++)
            {
                if (Apsim.FullPath(allModels[i]) == this.view.CurrentNodePath)
                {
                    index = i;
                    break;
                }
            }

            if (index == -1)
            {
                throw new Exception("Cannot find the current selected model in the .apsimx file");
            }

            // If the current model is the last one in the list then return false.
            if (index >= allModels.Count - 1)
            {
                return false;
            }

            // Select the next node.
            this.view.CurrentNodePath = Apsim.FullPath(allModels[index + 1]);
            return true;
        }

        /// <summary>
        /// String must have all alpha numeric or '_' characters
        /// </summary>
        /// <param name="str">Name to be checked</param>
        /// <returns>True if all chars are alphanumerics and <code>str</code> is not null</returns>
        public bool IsValidName(string str)
        {
            bool valid = true;

            // test for invalid characters
            if (!string.IsNullOrEmpty(str))
            {
                int i = 0;
                while (valid && (i < str.Length))
                {
                    if (!char.IsLetter(str[i]) && !char.IsNumber(str[i]) && (str[i] != '_') && (str[i] != ' '))
                    {
                        valid = false;
                    }

                    i++;
                }
            }
            else
            {
                valid = false;
            }

            return valid;
        }

        /// <summary>
        /// Rename the current node.
        /// </summary>
        public void Rename()
        {
            this.view.RenameCurrentNode();
        }

        #region Events from view

        /// <summary>
        /// A node has been dragged over another node. Allow drop?
        /// </summary>
        /// <param name="sender">Sending node</param>
        /// <param name="e">Node arguments</param>
        public void OnAllowDrop(object sender, AllowDropArgs e)
        {
            e.Allow = false;

            Model destinationModel = Apsim.Get(this.ApsimXFile, e.NodePath) as Model;
            if (destinationModel != null)
            {
                DragObject dragObject = e.DragObject as DragObject;
                ValidParentAttribute validParent = ReflectionUtilities.GetAttribute(dragObject.ModelType, typeof(ValidParentAttribute), false) as ValidParentAttribute;
                if (validParent == null || validParent.ParentModels.Length == 0)
                {
                    e.Allow = true;
                }
                else
                {
                    foreach (Type allowedParentType in validParent.ParentModels)
                    {
                        if (allowedParentType == destinationModel.GetType())
                        {
                            e.Allow = true;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// The view wants us to return a list of menu descriptions for the 
        /// main menu. 
        /// </summary>
        /// <param name="sender">Sender object</param>
        /// <param name="e">Event arguments</param>
        private void OnPopulateMainMenu(object sender, MenuDescriptionArgs e)
        {
            // Go look for all [UserInterfaceAction]
            foreach (MethodInfo method in typeof(MainMenu).GetMethods())
            {
                MainMenuAttribute mainMenuName = ReflectionUtilities.GetAttribute(method, typeof(MainMenuAttribute), false) as MainMenuAttribute;
                if (mainMenuName != null)
                {
                    MenuDescriptionArgs.Description desc = new MenuDescriptionArgs.Description();
                    desc.Name = mainMenuName.MenuName;
                    desc.ResourceNameForImage = "UserInterface.Resources.MenuImages." + desc.Name + ".png";

                    EventHandler handler = (EventHandler)Delegate.CreateDelegate(typeof(EventHandler), this.mainMenu, method);
                    desc.OnClick = handler;

                    e.Descriptions.Add(desc);
                }
            }
        }

        /// <summary>
        /// The view wants us to return a list of menu descriptions for the 
        /// currently selected Node.
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Event menu arguments</param>
        private void OnPopulateContextMenu(object sender, MenuDescriptionArgs e)
        {
            // Get the selected model.
            object selectedModel = Apsim.Get(this.ApsimXFile, this.view.CurrentNodePath);

            // Go look for all [UserInterfaceAction]
            foreach (MethodInfo method in typeof(ContextMenu).GetMethods())
            {
                ContextMenuAttribute contextMenuAttr = ReflectionUtilities.GetAttribute(method, typeof(ContextMenuAttribute), false) as ContextMenuAttribute;
                if (contextMenuAttr != null)
                {
                    bool ok = true;
                    if (contextMenuAttr.AppliesTo != null)
                    {
                        ok = false;
                        foreach (Type t in contextMenuAttr.AppliesTo)
                        {
                            if (t.IsAssignableFrom(selectedModel.GetType()))
                            {
                                ok = true;
                            }
                        }
                    }

                    if (ok)
                    {
                        MenuDescriptionArgs.Description desc = new MenuDescriptionArgs.Description();
                        desc.Name = contextMenuAttr.MenuName;
                        desc.ResourceNameForImage = "UserInterface.Resources.MenuImages." + desc.Name + ".png";
                        desc.ShortcutKey = contextMenuAttr.ShortcutKey;

                        // Check for an enabled method.
                        MethodInfo enabledMethod = typeof(ContextMenu).GetMethod(desc.ResourceNameForImage + "Enabled");
                        if (enabledMethod != null)
                        {
                            desc.Enabled = (bool)enabledMethod.Invoke(this.contextMenu, null);
                        }
                        else
                        {
                            desc.Enabled = true;
                        }

                        EventHandler handler = (EventHandler)Delegate.CreateDelegate(typeof(EventHandler), this.contextMenu, method);
                        desc.OnClick = handler;

                        if (desc.Name == "Advanced mode")
                        {
                            desc.Checked = this.advancedMode;
                        }

                        e.Descriptions.Add(desc);
                    }
                }
            }
        }
                
        /// <summary>
        /// The view wants us to populate the view with child nodes of the specified NodePath. 
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Node arguments</param>
        private void OnPopulateNodes(object sender, NodeDescriptionArgs e)
        {
            if (e.NodePath == null)
            {
                // Add in a root node.
                e.Descriptions.Add(this.GetNodeDescription(this.ApsimXFile));
            }
            else
            {
                Model model = Apsim.Get(this.ApsimXFile, e.NodePath) as Model;
                if (model != null)
                {
                    foreach (Model childModel in model.Children)
                    {
                        if (!childModel.IsHidden)
                        {
                            e.Descriptions.Add(this.GetNodeDescription(childModel));
                        }
                    }
                }
            }
        }

        /// <summary>
        /// User has selected a node - store and execute a SelectNodeCommand
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Node arguments</param>
        private void OnNodeSelectedByUser(object sender, NodeSelectedArgs e)
        {
            SelectNodeCommand cmd = new SelectNodeCommand(this.view, e.OldNodePath, e.NewNodePath);
            CommandHistory.Add(cmd, true);
            this.OnNodeSelected(sender, e);
        }

        /// <summary>
        /// A node has been selected (whether by user or undo/redo) 
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Node arguments</param>
        private void OnNodeSelected(object sender, NodeSelectedArgs e)
        {
            this.HideRightHandPanel();
            this.ShowRightHandPanel();
        }

        /// <summary>
        /// A node has begun to be dragged.
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Drag arguments</param>
        private void OnDragStart(object sender, DragStartArgs e)
        {
            Model obj = Apsim.Get(this.ApsimXFile, e.NodePath) as Model;
            if (obj != null)
            {
                string xml = Apsim.Serialise(obj);
                Clipboard.SetText(xml);

                DragObject dragObject = new DragObject();
                dragObject.NodePath = e.NodePath;
                dragObject.ModelType = obj.GetType();
                dragObject.Xml = xml;
                e.DragObject = dragObject;
            }
        }
        
        /// <summary>
        /// A node has been dropped. 
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Drop arguments</param>
        private void OnDrop(object sender, DropArgs e)
        {
            string toParentPath = e.NodePath;
            Model toParent = Apsim.Get(this.ApsimXFile, toParentPath) as Model;

            DragObject dragObject = e.DragObject as DragObject;
            if (dragObject != null && toParent != null)
            {
                string fromModelXml = dragObject.Xml;
                string fromParentPath = StringUtilities.ParentName(dragObject.NodePath);

                ICommand cmd = null;
                if (e.Copied)
                {
                    cmd = new AddModelCommand(fromModelXml, toParent);
                }
                else if (e.Moved)
                {
                    if (fromParentPath != toParentPath)
                    {
                        Model fromModel = Apsim.Get(this.ApsimXFile, dragObject.NodePath) as Model;
                        if (fromModel != null)
                        {
                            cmd = new MoveModelCommand(fromModel, toParent);
                        }
                    }
                }

                if (cmd != null)
                {
                    CommandHistory.Add(cmd);
                }
            }
        }

        /// <summary>
        /// User has renamed a node.
        /// </summary>
        /// <param name="sender">Sending object</param>
        /// <param name="e">Event node arguments</param>
        private void OnRename(object sender, NodeRenameArgs e)
        {
            e.CancelEdit = false;
            if (e.NewName != null)
            {
                if (this.IsValidName(e.NewName))
                {
                    Model model = Apsim.Get(this.ApsimXFile, e.NodePath) as Model;
                    if (model != null && model.GetType().Name != "Simulations" /*&& e.NewName != null*/ && e.NewName != string.Empty)
                    {
                        this.HideRightHandPanel();
                        string parentModelPath = StringUtilities.ParentName(e.NodePath);
                        RenameModelCommand cmd = new RenameModelCommand(model, parentModelPath, e.NewName);
                        CommandHistory.Add(cmd);
                        this.view.CurrentNodePath = parentModelPath + "." + e.NewName;
                        this.ShowRightHandPanel();
                        e.CancelEdit = true;
                    }
                }
                else
                {
                    this.ShowMessage("Use alpha numeric characters only!", DataStore.ErrorLevel.Error);
                    e.CancelEdit = true;
                }
            }
        }

        /// <summary>
        /// User has attempted to move the current node up.
        /// </summary>
        /// <param name="sender">Sender object</param>
        /// <param name="e">Event arguments</param>
        private void OnMoveUp(object sender, EventArgs e)
        {
            Model model = Apsim.Get(this.ApsimXFile, this.view.CurrentNodePath) as Model;
            
            if (model != null && model.Parent != null)
            {
                IModel firstModel = model.Parent.Children[0];
                if (model != firstModel)
                {
                    CommandHistory.Add(new Commands.MoveModelUpDownCommand(this.view, model, up: true));
                }
            }
        }

        /// <summary>
        /// User has attempted to move the current node down.
        /// </summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The args</param>
        private void OnMoveDown(object sender, EventArgs e)
        {
            Model model = Apsim.Get(this.ApsimXFile, this.view.CurrentNodePath) as Model;

            if (model != null && model.Parent != null)
            {
                IModel lastModel = model.Parent.Children[model.Parent.Children.Count - 1];
                if (model != lastModel)
                {
                    CommandHistory.Add(new Commands.MoveModelUpDownCommand(this.view, model, up: false));
                }
            }
        }

        /// <summary>
        /// User has pressed one of our shortcut keys.
        /// </summary>
        /// <param name="sender">Event sender</param>
        /// <param name="e">Event arguments</param>
        private void OnShortcutKeyPress(object sender, KeysArgs e)
        {
            if (e.Keys == Keys.F5)
            {
                ContextMenu contextMenu = new ContextMenu(this);
                contextMenu.RunAPSIM(sender, null);
            }
        }
        #endregion

        #region Privates 
        
        /// <summary>
        /// Write all errors thrown during the loading of the <code>.apsimx</code> file.
        /// </summary>
        private void WriteLoadErrors()
        {
            if (this.ApsimXFile.LoadErrors != null)
            {
                foreach (Exception err in this.ApsimXFile.LoadErrors)
                {
                    string message = string.Empty;
                    if (err is ApsimXException)
                    {
                        message = string.Format("[{0}]: {1}", (err as ApsimXException).model, err.Message);
                    }
                    else
                    {
                        if (!string.IsNullOrEmpty(err.Source) && err.Source != "mscorlib")
                            message = "[" + err.Source + "]: ";
                        message += string.Format("{0}", err.Message + "\r\n" + err.StackTrace);
                    }
                    if (err.InnerException != null)
                        message += "\r\n" + err.InnerException.Message;

                    this.view.ShowMessage(message, DataStore.ErrorLevel.Error);
                }
            }
        }
        
        /// <summary>
        /// A helper function for creating a node description object for the specified model. 
        /// </summary>
        /// <param name="model">The model</param>
        /// <returns>The description</returns>
        private NodeDescriptionArgs.Description GetNodeDescription(IModel model)
        {
            NodeDescriptionArgs.Description description = new NodeDescriptionArgs.Description();
            description.Name = model.Name;

            string imageFileName;
            if (model is ModelCollectionFromResource)
                imageFileName = (model as ModelCollectionFromResource).ResourceName;
            else if (model.GetType().Name == "Plant" || model.GetType().Name == "OldPlant")
                imageFileName = model.Name;
            else
                imageFileName = model.GetType().Name;

            description.ResourceNameForImage = "UserInterface.Resources.TreeViewImages." + imageFileName + ".png";
            description.HasChildren = this.SomeChildrenVisible(model);
            return description;
        }

        /// <summary>
        /// Returns true if some children of the specified model are visible (not hidden)
        /// </summary>
        /// <param name="model">Look at this models children</param>
        /// <returns>True if some are visible</returns>
        private bool SomeChildrenVisible(IModel model)
        {
            foreach (Model child in model.Children)
            {
                if (!child.IsHidden)
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Hide the right hand panel.
        /// </summary>
        private void HideRightHandPanel()
        {
            if (this.currentRightHandPresenter != null)
            {
                try
                {
                    this.currentRightHandPresenter.Detach();
                    this.currentRightHandPresenter = null;
                }
                catch (Exception err)
                {
                    this.ShowMessage(err.Message, DataStore.ErrorLevel.Error);
                }
            }

            this.view.AddRightHandView(null);
        }

        /// <summary>
        /// Display a view on the right hand panel in view.
        /// </summary>
        private void ShowRightHandPanel()
        {
            if (this.view.CurrentNodePath != string.Empty)
            {
                object model = Apsim.Get(this.ApsimXFile, this.view.CurrentNodePath);

                if (model != null)
                {
                    ViewNameAttribute viewName = ReflectionUtilities.GetAttribute(model.GetType(), typeof(ViewNameAttribute), false) as ViewNameAttribute;
                    PresenterNameAttribute presenterName = ReflectionUtilities.GetAttribute(model.GetType(), typeof(PresenterNameAttribute), false) as PresenterNameAttribute;

                    if (this.advancedMode)
                    {
                        viewName = new ViewNameAttribute("UserInterface.Views.GridView");
                        presenterName = new PresenterNameAttribute("UserInterface.Presenters.PropertyPresenter");
                    }

                    if (viewName != null && presenterName != null)
                    {
                        UserControl newView = Assembly.GetExecutingAssembly().CreateInstance(viewName.ToString()) as UserControl;
                        this.currentRightHandPresenter = Assembly.GetExecutingAssembly().CreateInstance(presenterName.ToString()) as IPresenter;
                        if (newView != null && this.currentRightHandPresenter != null)
                        {
                            try
                            {
                                this.view.AddRightHandView(newView);
                                this.currentRightHandPresenter.Attach(model, newView, this);
                            }
                            catch (Exception err)
                            {
                                string message = err.Message;
                                message += "\r\n" + err.StackTrace;
                                this.ShowMessage(message, DataStore.ErrorLevel.Error);
                            }
                        }
                    }
                }
            }
        }

        #endregion

        #region Events from model

        /// <summary>
        /// The model structure has changed. Tell the view about it. 
        /// </summary>
        /// <param name="modelPath">Path to the model file</param>
        private void OnModelStructureChanged(IModel model)
        {
            string modelPath = Apsim.FullPath(model);
            this.view.InvalidateNode(modelPath, this.GetNodeDescription(model));
        }

        #endregion
    }

    /// <summary>
    /// An object that encompasses the data that is dragged during a drag/drop operation.
    /// </summary>
    [Serializable]
    public class DragObject : ISerializable
    {
        /// <summary>
        /// Path to the node
        /// </summary>
        private string nodePath;

        /// <summary>
        /// Xml string
        /// </summary>
        private string xml;

        /// <summary>
        /// Type of the model
        /// </summary>
        private Type modelType;

        /// <summary>
        /// Gets or sets the path to the node
        /// </summary>
        public string NodePath
        {
            get { return this.nodePath; }
            set { this.nodePath = value; }
        }

        /// <summary>
        /// Gets or sets the xml string
        /// </summary>
        public string Xml
        {
            get { return this.xml; }
            set { this.xml = value; }
        }

        /// <summary>
        /// Gets or sets the type of model
        /// </summary>
        public Type ModelType
        {
            get { return this.modelType; }
            set { this.modelType = value; }
        }

        /// <summary>
        /// Get data for the specified object in the xml
        /// </summary>
        /// <param name="info">Serialized object</param>
        /// <param name="context">The context</param>
        void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("NodePath", this.NodePath);
            info.AddValue("Xml", this.Xml);
        }
    }
}
