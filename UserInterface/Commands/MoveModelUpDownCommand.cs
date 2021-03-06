﻿using UserInterface.Views;
using Models.Core;
using System.Xml;
using System;
using UserInterface.Interfaces;

namespace UserInterface.Commands
{
    /// <summary>
    /// This command moves a model up or down one spot in the siblings
    /// </summary>
    class MoveModelUpDownCommand : ICommand
    {
        private IExplorerView ExplorerView;
        private Model ModelToMove;
        private bool MoveUp;
        private bool ModelWasMoved;

        /// <summary>
        /// Constructor.
        /// </summary>
        public MoveModelUpDownCommand(IExplorerView explorerView, Model modelToMove, bool up)
        {
            ExplorerView = explorerView;
            ModelToMove = modelToMove;
            MoveUp = up;
        }

        /// <summary>
        /// Perform the command
        /// </summary>
        public void Do(CommandHistory CommandHistory)
        {
            Model parent = ModelToMove.Parent as Model;

            int modelIndex = parent.Children.IndexOf(ModelToMove);

            ModelWasMoved = false;
            if (MoveUp)
            {
                if (modelIndex != 0)
                    MoveModelUp(CommandHistory, parent, modelIndex);
            }
            else
            {
                if (modelIndex != parent.Children.Count - 1)
                    MoveModelDown(CommandHistory, parent, modelIndex);
            }
        }

        /// <summary>
        /// Undo the command
        /// </summary>
        public void Undo(CommandHistory CommandHistory)
        {
            if (ModelWasMoved)
            {
                Model parent = ModelToMove.Parent as Model;
                int modelIndex = parent.Children.IndexOf(ModelToMove);
                if (MoveUp)
                    MoveModelDown(CommandHistory, parent, modelIndex);
                else
                    MoveModelUp(CommandHistory, parent, modelIndex);
            }
        }


        private void MoveModelDown(CommandHistory CommandHistory, Model parent, int modelIndex)
        {
            parent.Children.Remove(ModelToMove);
            parent.Children.Insert(modelIndex + 1, ModelToMove);
            CommandHistory.InvokeModelStructureChanged(parent);
            ExplorerView.CurrentNodePath = Apsim.FullPath(ModelToMove);
            ModelWasMoved = true;
        }

        private void MoveModelUp(CommandHistory CommandHistory, Model parent, int modelIndex)
        {
            parent.Children.Remove(ModelToMove);
            parent.Children.Insert(modelIndex - 1, ModelToMove);
            CommandHistory.InvokeModelStructureChanged(parent);
            ExplorerView.CurrentNodePath = Apsim.FullPath(ModelToMove);
            ModelWasMoved = true;
        }

    }
}
