# Local Managed Identity

This project is a small service that you can run locally that emulates Azure Managed Identity by using Azure CLI to get access tokens.

This is useful when you are trying to run your software in Docker containers while developing, where the [Azure.Identity][1] SDK cannot access your Azure CLI login directly, and Integrated Windows Authentication is unavailable.

## How to Use
First, ensure you're logged into Azure CLI.

```
> az login
```

Then run the local-managed-identity executable:

```
> ./local-managed-identity
Local Managed Identity v1.0.0.0
---- PowerShell Environment Variables ----
$env:MSI_ENDPOINT = "http://localhost:5436/"
$env:MSI_SECRET = "c85fd760-4990-4363-a2fe-57d89d06711c"
------- Bash Environment Variables ------
export MSI_ENDPOINT="http://localhost:5436/"
export MSI_SECRET="c85fd760-4990-4363-a2fe-57d89d06711c"
-- Docker (Windows/Mac) Run Parameters --
-e MSI_ENDPOINT=http://host.docker.internal:5436/ -e MSI_SECRET
-----------------------------------------
Server started. Ctrl+C to quit.
```

Set the `MSI_ENDPOINT` and `MSI_SECRET` environment variables as specified in the terminal output. Any application using the Azure.Identity [ManagedIdentityCredential][2] will be looking for these environment variables and will then get its access tokens via Local Managed Identity. Local Managed Identity will in turn get the access tokens from Azure CLI using `az account get-access-token ...`.

If you're running your application from inside Docker container, you'll need to use `host.docker.internal` instead of `localhost` on Windows and Mac systems to ensure you get the host PC's IP address correctly inside the container. The terminal output shows example parameters you can pass to `docker run`.

## How to Build
This project uses [Stack](https://haskellstack.org/) to build.

```
> stack setup
> stack build
```
The `setup` command is only required the first time to ensure you have the correct version of GHC installed.


[1]: https://github.com/Azure/azure-sdk-for-net/tree/main/sdk/identity/Azure.Identity
[2]: https://docs.microsoft.com/en-au/dotnet/api/azure.identity.managedidentitycredential?view=azure-dotnet
