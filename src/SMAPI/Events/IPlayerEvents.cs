using System;

namespace StardewModdingAPI.Events
{
    /// <summary>Events raised when a player's data changes. Formerly <c>IPlayerEvents</c>.</summary>
    public interface IPlayerEvents
    {
        /// <summary>Raised after items are added to or removed from a player's inventory.</summary>
        event EventHandler<InventoryChangedEventArgs> InventoryChanged;

        /// <summary>Raised after a player skill level changes. This happens as soon as they level up, not when the game notifies the player after their character goes to bed.</summary>
        event EventHandler<LevelChangedEventArgs> LevelChanged;

        /// <summary>Raised after a player warps to a new location.</summary>
        event EventHandler<WarpedEventArgs> Warped;
    }
}
