using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Azure;
using Azure.Storage.Blobs;
using Azure.Storage.Blobs.Models;
using Microsoft.Extensions.Options;
using StardewModdingAPI.Web.Framework.Clients.Pastebin;
using StardewModdingAPI.Web.Framework.Compression;
using StardewModdingAPI.Web.Framework.ConfigModels;

namespace StardewModdingAPI.Web.Framework.Storage
{
    /// <summary>Provides access to raw data storage.</summary>
    internal class StorageProvider : IStorageProvider
    {
        /*********
        ** Fields
        *********/
        /// <summary>The API client settings.</summary>
        private readonly ApiClientsConfig ClientsConfig;

        /// <summary>The underlying Pastebin client.</summary>
        private readonly IPastebinClient Pastebin;

        /// <summary>The underlying text compression helper.</summary>
        private readonly IGzipHelper GzipHelper;

        /// <summary>Whether Azure blob storage is configured.</summary>
        private bool HasAzure => !string.IsNullOrWhiteSpace(this.ClientsConfig.AzureBlobConnectionString);

        /// <summary>The number of days since the blob's last-modified date when it will be deleted.</summary>
        private int ExpiryDays => this.ClientsConfig.AzureBlobTempExpiryDays;


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="clientsConfig">The API client settings.</param>
        /// <param name="pastebin">The underlying Pastebin client.</param>
        /// <param name="gzipHelper">The underlying text compression helper.</param>
        public StorageProvider(IOptions<ApiClientsConfig> clientsConfig, IPastebinClient pastebin, IGzipHelper gzipHelper)
        {
            this.ClientsConfig = clientsConfig.Value;
            this.Pastebin = pastebin;
            this.GzipHelper = gzipHelper;
        }

        /// <summary>Save a text file to storage.</summary>
        /// <param name="content">The content to upload.</param>
        /// <param name="compress">Whether to gzip the text.</param>
        /// <returns>Returns metadata about the save attempt.</returns>
        public async Task<UploadResult> SaveAsync(string content, bool compress = true)
        {
            string id = Guid.NewGuid().ToString("N");

            // save to Azure
            if (this.HasAzure)
            {
                try
                {
                    using Stream stream = new MemoryStream(Encoding.UTF8.GetBytes(content));
                    BlobClient blob = this.GetAzureBlobClient(id);
                    await blob.UploadAsync(stream);

                    return new UploadResult(true, id, null);
                }
                catch (Exception ex)
                {
                    return new UploadResult(false, null, ex.Message);
                }
            }

            // save to local filesystem for testing
            else
            {
                string path = this.GetDevFilePath(id);
                Directory.CreateDirectory(Path.GetDirectoryName(path));

                File.WriteAllText(path, content);
                return new UploadResult(true, id, null);
            }
        }

        /// <summary>Fetch raw text from storage.</summary>
        /// <param name="id">The storage ID returned by <see cref="SaveAsync"/>.</param>
        public async Task<StoredFileInfo> GetAsync(string id)
        {
            // fetch from blob storage
            if (Guid.TryParseExact(id, "N", out Guid _))
            {
                // Azure Blob storage
                if (this.HasAzure)
                {
                    try
                    {
                        BlobClient blob = this.GetAzureBlobClient(id);
                        Response<BlobDownloadInfo> response = await blob.DownloadAsync();
                        using BlobDownloadInfo result = response.Value;

                        using StreamReader reader = new StreamReader(result.Content);
                        DateTimeOffset expiry = result.Details.LastModified + TimeSpan.FromDays(this.ExpiryDays);
                        string content = this.GzipHelper.DecompressString(reader.ReadToEnd());

                        return new StoredFileInfo
                        {
                            Success = true,
                            Content = content,
                            Expiry = expiry.UtcDateTime
                        };
                    }
                    catch (RequestFailedException ex)
                    {
                        return new StoredFileInfo
                        {
                            Error = ex.ErrorCode == "BlobNotFound"
                                ? "There's no file with that ID."
                                : $"Could not fetch that file from storage ({ex.ErrorCode}: {ex.Message})."
                        };
                    }
                }

                // local filesystem for testing
                else
                {
                    FileInfo file = new FileInfo(this.GetDevFilePath(id));
                    if (file.Exists)
                    {
                        if (file.LastWriteTimeUtc.AddDays(this.ExpiryDays) < DateTime.UtcNow)
                            file.Delete();
                        else
                        {
                            return new StoredFileInfo
                            {
                                Success = true,
                                Content = File.ReadAllText(file.FullName),
                                Expiry = DateTime.UtcNow.AddDays(this.ExpiryDays),
                                Warning = "This file was saved temporarily to the local computer. This should only happen in a local development environment."
                            };
                        }
                    }
                    return new StoredFileInfo
                    {
                        Error = "There's no file with that ID."
                    };
                }
            }

            // get from Pastebin
            else
            {
                PasteInfo response = await this.Pastebin.GetAsync(id);
                response.Content = this.GzipHelper.DecompressString(response.Content);
                return new StoredFileInfo
                {
                    Success = response.Success,
                    Content = response.Content,
                    Error = response.Error
                };
            }
        }

        /// <summary>Get a client for reading and writing to Azure Blob storage.</summary>
        /// <param name="id">The file ID.</param>
        private BlobClient GetAzureBlobClient(string id)
        {
            var azure = new BlobServiceClient(this.ClientsConfig.AzureBlobConnectionString);
            var container = azure.GetBlobContainerClient(this.ClientsConfig.AzureBlobTempContainer);
            return container.GetBlobClient($"uploads/{id}");
        }

        /// <summary>Get the absolute file path for an upload when running in a local test environment with no Azure account configured.</summary>
        /// <param name="id">The file ID.</param>
        private string GetDevFilePath(string id)
        {
            return Path.Combine(Path.GetTempPath(), "smapi-web-temp", $"{id}.txt");
        }
    }
}
