using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewModdingAPI.Web.ViewModels.JsonValidator
{
    /// <summary>The view model for the JSON validator page.</summary>
    public class JsonValidatorModel
    {
        /*********
        ** Accessors
        *********/
        /// <summary>The paste ID.</summary>
        public string PasteID { get; set; }

        /// <summary>The schema name with which the JSON was validated.</summary>
        public string SchemaName { get; set; }

        /// <summary>The supported JSON schemas (names indexed by ID).</summary>
        public readonly IDictionary<string, string> SchemaFormats;

        /// <summary>The validated content.</summary>
        public string Content { get; set; }

        /// <summary>The schema validation errors, if any.</summary>
        public JsonValidatorErrorModel[] Errors { get; set; } = new JsonValidatorErrorModel[0];

        /// <summary>A non-blocking warning while uploading the file.</summary>
        public string UploadWarning { get; set; }

        /// <summary>When the uploaded file will no longer be available.</summary>
        public DateTime? Expiry { get; set; }

        /// <summary>An error which occurred while uploading the JSON.</summary>
        public string UploadError { get; set; }

        /// <summary>An error which occurred while parsing the JSON.</summary>
        public string ParseError { get; set; }

        /// <summary>A web URL to the user-facing format documentation.</summary>
        public string FormatUrl { get; set; }


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        public JsonValidatorModel() { }

        /// <summary>Construct an instance.</summary>
        /// <param name="pasteID">The stored file ID.</param>
        /// <param name="schemaName">The schema name with which the JSON was validated.</param>
        /// <param name="schemaFormats">The supported JSON schemas (names indexed by ID).</param>
        public JsonValidatorModel(string pasteID, string schemaName, IDictionary<string, string> schemaFormats)
        {
            this.PasteID = pasteID;
            this.SchemaName = schemaName;
            this.SchemaFormats = schemaFormats;
        }

        /// <summary>Set the validated content.</summary>
        /// <param name="content">The validated content.</param>
        /// <param name="expiry">When the uploaded file will no longer be available.</param>
        /// <param name="uploadWarning">A non-blocking warning while uploading the log.</param>
        public JsonValidatorModel SetContent(string content, DateTime? expiry, string uploadWarning = null)
        {
            this.Content = content;
            this.Expiry = expiry;
            this.UploadWarning = uploadWarning;

            return this;
        }

        /// <summary>Set the error which occurred while uploading the JSON.</summary>
        /// <param name="error">The error message.</param>
        public JsonValidatorModel SetUploadError(string error)
        {
            this.UploadError = error;

            return this;
        }

        /// <summary>Set the error which occurred while parsing the JSON.</summary>
        /// <param name="error">The error message.</param>
        public JsonValidatorModel SetParseError(string error)
        {
            this.ParseError = error;

            return this;
        }

        /// <summary>Add validation errors to the response.</summary>
        /// <param name="errors">The schema validation errors.</param>
        public JsonValidatorModel AddErrors(params JsonValidatorErrorModel[] errors)
        {
            this.Errors = this.Errors.Concat(errors).ToArray();

            return this;
        }
    }
}
