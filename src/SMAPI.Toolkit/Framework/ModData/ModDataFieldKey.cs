namespace StardewModdingAPI.Toolkit.Framework.ModData
{
    /// <summary>The valid field keys.</summary>
    public enum ModDataFieldKey
    {
        /// <summary>A manifest update key.</summary>
        UpdateKey,

        /// <summary>An alternative URL the player can check for an updated version.</summary>
        AlternativeUrl,

        /// <summary>The mod's predefined compatibility status.</summary>
        Status,

        /// <summary>A reason phrase for the <see cref="Status"/>, or <c>null</c> to use the default reason.</summary>
        StatusReasonPhrase
    }
}
