using System.Collections.Generic;
using Microsoft.Xna.Framework;
using StardewValley;
using StardewValley.Buildings;
using StardewValley.TerrainFeatures;

namespace StardewModdingAPI.Framework.StateTracking.Snapshots
{
    /// <summary>A frozen snapshot of a tracked game location.</summary>
    internal class LocationSnapshot
    {
        /*********
        ** Accessors
        *********/
        /// <summary>The tracked location.</summary>
        public GameLocation Location { get; }

        /// <summary>Tracks added or removed buildings.</summary>
        public SnapshotListDiff<Building> Buildings { get; } = new SnapshotListDiff<Building>();

        /// <summary>Tracks added or removed debris.</summary>
        public SnapshotListDiff<Debris> Debris { get; } = new SnapshotListDiff<Debris>();

        /// <summary>Tracks added or removed large terrain features.</summary>
        public SnapshotListDiff<LargeTerrainFeature> LargeTerrainFeatures { get; } = new SnapshotListDiff<LargeTerrainFeature>();

        /// <summary>Tracks added or removed NPCs.</summary>
        public SnapshotListDiff<NPC> Npcs { get; } = new SnapshotListDiff<NPC>();

        /// <summary>Tracks added or removed objects.</summary>
        public SnapshotListDiff<KeyValuePair<Vector2, Object>> Objects { get; } = new SnapshotListDiff<KeyValuePair<Vector2, Object>>();

        /// <summary>Tracks added or removed terrain features.</summary>
        public SnapshotListDiff<KeyValuePair<Vector2, TerrainFeature>> TerrainFeatures { get; } = new SnapshotListDiff<KeyValuePair<Vector2, TerrainFeature>>();


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="location">The tracked location.</param>
        public LocationSnapshot(GameLocation location)
        {
            this.Location = location;
        }

        /// <summary>Update the tracked values.</summary>
        /// <param name="watcher">The watcher to snapshot.</param>
        public void Update(LocationTracker watcher)
        {
            this.Buildings.Update(watcher.BuildingsWatcher);
            this.Debris.Update(watcher.DebrisWatcher);
            this.LargeTerrainFeatures.Update(watcher.LargeTerrainFeaturesWatcher);
            this.Npcs.Update(watcher.NpcsWatcher);
            this.Objects.Update(watcher.ObjectsWatcher);
            this.TerrainFeatures.Update(watcher.TerrainFeaturesWatcher);
        }
    }
}
