﻿// -----------------------------------------------------------------------
// <copyright file="IExplorerView.cs" company="APSIM Initiative">
//     Copyright (c) APSIM Initiative
// </copyright>
// -----------------------------------------------------------------------
namespace UserInterface.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Runtime.Serialization;
    using System.Windows.Forms;
    using EventArguments;

    /// <summary>
    /// A structure for holding info about an item in the treeview.
    /// </summary>
    public class NodeDescriptionArgs : EventArgs
    {
        public struct Description
        {
            public string Name;
            public string ResourceNameForImage;
            public bool HasChildren;
        }
        /// <summary>
        /// The path of the node that needs child descriptions. If this is null then
        /// Descriptions needs to contain root nodes.
        /// </summary>
        public string NodePath;
        public List<Description> Descriptions = new List<Description>();
    }

    /// <summary>
    /// A class for holding info about a collection of menu items.
    /// </summary>
    public class MenuDescriptionArgs : EventArgs
    {
        public struct Description
        {
            public string Name;
            public string ResourceNameForImage;
            public EventHandler OnClick;
            public bool Checked;
            public Keys ShortcutKey;
            public bool Enabled;
        }

        public List<Description> Descriptions = new List<Description>();
    }

    /// <summary>
    /// A class for holding info about a node selection event.
    /// </summary>
    public class NodeSelectedArgs : EventArgs
    {
        public string OldNodePath;
        public string NewNodePath;
    }

    /// <summary>
    /// A clas for holding info about a node rename event.
    /// </summary>
    public class NodeRenameArgs : EventArgs
    {
        public string NodePath;
        public string NewName;
        public bool CancelEdit;
    }

    /// <summary>
    /// A class for holding info about a begin drag event.
    /// </summary>
    public class DragStartArgs : EventArgs
    {
        public string NodePath;
        public ISerializable DragObject;
    }

    /// <summary>
    /// A class for holding info about a begin drag event.
    /// </summary>
    public class AllowDropArgs : EventArgs
    {
        public string NodePath;
        public ISerializable DragObject;
        public bool Allow;
    }

    /// <summary>
    /// A class for holding info about a begin drag event.
    /// </summary>
    public class DropArgs : EventArgs
    {
        public string NodePath;
        public bool Copied;
        public bool Moved;
        public bool Linked;
        public ISerializable DragObject;
    }

    /// <summary>
    /// The interface for an explorer view.
    /// NB: All node paths are compatible with XmlHelper node paths.
    /// e.g.  /simulations/test/clock
    /// </summary>
    public interface IExplorerView
    {
        /// <summary>
        /// ExplorerView will invoke this event when it wants the presenter to populate 
        /// direct children of the specified node.
        /// </summary>
        event EventHandler<NodeDescriptionArgs> PopulateChildNodes;

        /// <summary>
        /// This event will be invoked when the user selects a node.
        /// </summary>
        event EventHandler<NodeSelectedArgs> NodeSelectedByUser;

        /// <summary>
        /// This event will be invoked when a node is selected not by the user
        /// but by an Undo command.
        /// </summary>
        event EventHandler<NodeSelectedArgs> NodeSelected;

        /// <summary>
        /// ExplorerView will invoke this event when it wants the presenter to populate 
        /// the main menu with items.
        /// </summary>
        event EventHandler<MenuDescriptionArgs> PopulateMainMenu;

        /// <summary>
        /// ExplorerView will invoke this event when it wants the presenter to populate
        /// the context (popup) menu for the specified node.
        /// </summary>
        event EventHandler<MenuDescriptionArgs> PopulateContextMenu;

        /// <summary>
        /// Invoked when a drag operation has commenced. Need to create a DragObject.
        /// </summary>
        event EventHandler<DragStartArgs> DragStart;

        /// <summary>
        /// Invoked when the view wants to know if a drop is allowed on the specified Node.
        /// </summary>
        event EventHandler<AllowDropArgs> AllowDrop;

        /// <summary>
        /// Invoked when a drop has occurred.
        /// </summary>
        event EventHandler<DropArgs> Drop;

        /// <summary>
        /// Invoked then a node is renamed.
        /// </summary>
        event EventHandler<NodeRenameArgs> Rename;

        /// <summary>
        /// Invoked then a node is moved down
        /// </summary>
        event EventHandler<EventArgs> OnMoveDown;

        /// <summary>
        /// Invoked then a node is moved up
        /// </summary>
        event EventHandler<EventArgs> OnMoveUp;

        /// <summary>
        /// Invoked when a shortcut key is pressed.
        /// </summary>
        event EventHandler<KeysArgs> OnShortcutKeyPress;

        /// <summary>
        /// Return the current node path.
        /// </summary>
        string CurrentNodePath { get; set; }

        /// <summary>
        /// Gets or sets the shortcut keys.
        /// </summary>
        Keys[] ShortcutKeys { get; set; }

        /// <summary>
        /// Invalidate (redraw) the specified node and its direct child nodes.
        /// </summary>
        void InvalidateNode(string NodePath, NodeDescriptionArgs.Description Description);

        /// <summary>
        /// Add a view to the right hand panel.
        /// </summary>
        void AddRightHandView(UserControl Control);

        /// <summary>
        /// Rename the current node.
        /// </summary>
        void RenameCurrentNode();

        /// <summary>
        /// Ask about saving.
        /// </summary>
        /// <returns>-1, 0, 1</returns>
        Int32 AskToSave();

        /// <summary>
        /// A helper function that asks user for a folder.
        /// </summary>
        /// <returns>Returns the selected folder or null if action cancelled by user.</returns>
        string AskUserForFolder(string prompt);

        /// <summary>
        /// Add a status message. A message of null will clear the status message.
        /// </summary>
        /// <param name="Message"></param>
        void ShowMessage(string Message, Models.DataStore.ErrorLevel errorLevel);

        /// <summary>
        /// A helper function that asks user for a SaveAs name and returns their new choice.
        /// </summary>
        string SaveAs(string OldFilename);

        /// <summary>
        /// Change the name of the tab.
        /// </summary>
        void ChangeTabText(string NewTabName);

        /// <summary>
        /// Turn on or off the 2nd explorer view.
        /// </summary>
        void ToggleSecondExplorerViewVisible();

        Int32 TreeWidth { get; set; }
    }


}
