using System;
using System.Collections.Generic;
using System.Linq;
using StardewModdingAPI.Enums;
using StardewModdingAPI.Events;
using StardewValley;

namespace StardewModdingAPI.Framework.StateTracking.Snapshots
{
    /// <summary>A frozen snapshot of a tracked player.</summary>
    internal class PlayerSnapshot
    {
        /*********
        ** Accessors
        *********/
        /// <summary>The player being tracked.</summary>
        public Farmer Player { get; }

        /// <summary>The player's current location.</summary>
        public SnapshotDiff<GameLocation> Location { get; } = new SnapshotDiff<GameLocation>();

        /// <summary>Tracks changes to the player's skill levels.</summary>
        public IDictionary<SkillType, SnapshotDiff<int>> Skills { get; } =
            Enum
                .GetValues(typeof(SkillType))
                .Cast<SkillType>()
                .ToDictionary(skill => skill, skill => new SnapshotDiff<int>());

        /// <summary>Get a list of inventory changes.</summary>
        public IEnumerable<ItemStackChange> InventoryChanges { get; private set; }


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="player">The player being tracked.</param>
        public PlayerSnapshot(Farmer player)
        {
            this.Player = player;
        }

        /// <summary>Update the tracked values.</summary>
        /// <param name="watcher">The player watcher to snapshot.</param>
        public void Update(PlayerTracker watcher)
        {
            this.Location.Update(watcher.LocationWatcher);
            foreach (var pair in this.Skills)
                pair.Value.Update(watcher.SkillWatchers[pair.Key]);
            this.InventoryChanges = watcher.GetInventoryChanges().ToArray();
        }
    }
}
