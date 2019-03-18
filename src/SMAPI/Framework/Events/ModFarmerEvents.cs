using System;
using StardewModdingAPI.Events;

namespace StardewModdingAPI.Framework.Events
{
    /// <summary>Events raised when the farmer's data changes.</summary>
    internal class ModFarmerEvents : ModEventsBase, IFarmerEvents
    {
        /*********
        ** Accessors
        *********/
        /// <summary>Raised after items are added or removed to a farmer's inventory.</summary>
        public event EventHandler<InventoryChangedEventArgs> InventoryChanged
        {
            add => this.EventManager.InventoryChanged.Add(value);
            remove => this.EventManager.InventoryChanged.Remove(value);
        }

        /// <summary>Raised after a farmer skill level changes. This happens as soon as they level up, not when the game notifies the player after their character goes to bed.</summary>
        public event EventHandler<LevelChangedEventArgs> LevelChanged
        {
            add => this.EventManager.LevelChanged.Add(value);
            remove => this.EventManager.LevelChanged.Remove(value);
        }

        /// <summary>Raised after a farmer warps to a new location.</summary>
        public event EventHandler<WarpedEventArgs> Warped
        {
            add => this.EventManager.Warped.Add(value);
            remove => this.EventManager.Warped.Remove(value);
        }


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="mod">The mod which uses this instance.</param>
        /// <param name="eventManager">The underlying event manager.</param>
        internal ModFarmerEvents(IModMetadata mod, EventManager eventManager)
            : base(mod, eventManager) { }
    }
}
