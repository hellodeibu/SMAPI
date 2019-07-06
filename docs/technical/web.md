&larr; [README](../README.md)

**SMAPI.Web** contains the code for the `smapi.io` website, including the mod compatibility list
and update check API.

## Contents
* [Overview](#overview)
  * [Log parser](#log-parser)
  * [Web API](#web-api)
* [For SMAPI developers](#for-smapi-developers)
  * [Local development](#local-development)
  * [Deploying to Amazon Beanstalk](#deploying-to-amazon-beanstalk)

## Overview
The `StardewModdingAPI.Web` project provides an API and web UI hosted at `*.smapi.io`.

### Log parser
The log parser provides a web UI for uploading, parsing, and sharing SMAPI logs. The logs are
persisted in a compressed form to Pastebin.

The log parser lives at https://log.smapi.io.

### Web API
SMAPI provides a web API at `api.smapi.io` for use by SMAPI and external tools. The URL includes a
`{version}` token, which is the SMAPI version for backwards compatibility. This API is publicly
accessible but not officially released; it may change at any time.

The API has one `/mods` endpoint. This provides mod info, including official versions and URLs
(from Chucklefish, GitHub, or Nexus), unofficial versions from the wiki, and optional mod metadata
from the wiki and SMAPI's internal data. This is used by SMAPI to perform update checks, and by
external tools to fetch mod data.

The API accepts a `POST` request with the mods to match, each of which **must** specify an ID and
may _optionally_ specify [update keys](https://stardewvalleywiki.com/Modding:Modder_Guide/APIs/Manifest#Update_checks).
The API will automatically try to fetch known update keys from the wiki and internal data based on
the given ID.

```
POST https://api.smapi.io/v2.0/mods
{
   "mods": [
      {
         "id": "Pathoschild.LookupAnything",
         "updateKeys": [ "nexus:541", "chucklefish:4250" ]
      }
   ],
   "includeExtendedMetadata": true
}
```

The API will automatically aggregate versions and errors. Each mod will include...
* an `id` (matching what you passed in);
* up to three versions: `main` (e.g. 'latest version' field on Nexus), `optional` if newer (e.g.
  optional files on Nexus), and `unofficial` if newer (from the wiki);
* `metadata` with mod info crossreferenced from the wiki and internal data (only if you specified
  `includeExtendedMetadata: true`);
* and `errors` containing any error messages that occurred while fetching data.

For example:
```
[
   {
      "id": "Pathoschild.LookupAnything",
      "main": {
         "version": "1.19",
         "url": "https://www.nexusmods.com/stardewvalley/mods/541"
      },
      "metadata": {
         "id": [
            "Pathoschild.LookupAnything",
            "LookupAnything"
         ],
         "name": "Lookup Anything",
         "nexusID": 541,
         "gitHubRepo": "Pathoschild/StardewMods",
         "compatibilityStatus": "Ok",
         "compatibilitySummary": "✓ use latest version."
      },
      "errors": []
   }
]
```

## For SMAPI developers
### Local development
`StardewModdingAPI.Web` is a regular ASP.NET MVC Core app, so you can just launch it from within
Visual Studio to run a local version.

There are two differences when it's run locally: all endpoints use HTTP instead of HTTPS, and the
subdomain portion becomes a route (e.g. `log.smapi.io` &rarr; `localhost:59482/log`).

Before running it locally, you need to enter your credentials in the `appsettings.Development.json`
file. See the next section for a description of each setting. This file is listed in `.gitignore`
to prevent accidentally committing credentials.

### Deploying to Amazon Beanstalk
The app can be deployed to a standard Amazon Beanstalk IIS environment. When creating the
environment, make sure to specify the following environment properties:

property name                   | description
------------------------------- | -----------------
`LogParser:PastebinDevKey`      | The [Pastebin developer key](https://pastebin.com/api#1) used to authenticate with the Pastebin API.
`LogParser:PastebinUserKey`     | The [Pastebin user key](https://pastebin.com/api#8) used to authenticate with the Pastebin API. Can be left blank to post anonymously.
`LogParser:SectionUrl`          | The root URL of the log page, like `https://log.smapi.io/`.
`ModUpdateCheck:GitHubPassword` | The password with which to authenticate to GitHub when fetching release info.
`ModUpdateCheck:GitHubUsername` | The username with which to authenticate to GitHub when fetching release info.
