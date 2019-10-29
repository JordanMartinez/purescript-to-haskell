# Yesod

This folder's contents were generated via `stack new my-yesod yesod-postgres`. Then they were edited by hand. This folder is still a WIP.

Changes made:
- Changed Stack's LTS resolver to 14.11
- Removed version boundaries on all depenedencies in `package.yml`
- Use meta-language to document Route syntax
- Use meta-language to document Persistent syntax
- Deleted files related to Shakespearean files (Hamlet, Julius, Cassius/Lucius):
		- Deleted `/template/*` files
		- Deleted `/static/*` files
		- Deleted `.dir-locals.el` file

## Overview of Directories and Files

```
/app -- source code for main.hs (live version) and devel.hs (test version)
		 -- no reason to ever touch these files once created

/config -- all configuration- and Template Haskell-related things
	/favicon.ico -- self-explanatory
	/keter.yml -- configuration for deploying this web application via Keter
	/models -- define your database's tables via Persistent's Template Haskell syntax
	/robots.txt -- self-explanatory
	/routes -- define your web server's routes via Template Haskell
	/settings.yml -- define settings for live version. For example
								-- - the location of the static directory
								-- - database configuration (e.g. username, password, etc.)
								-- - copyright statement
								-- - analytics code
	/test-settings.yml -- define settings for test version
/src
	/Handler -- All routes defined in `/config/routes`
					 -- will be handled in this folder
	/Import	 -- Directory for storing any files containing
					 -- any types/functions/values that should
					 -- be accessible throughout most/all of the application.
		/NoFoundation.hs -- Defines most of the imports
										 -- for Yesod and its dependencies
										 -- in one file.
	/Settings
		StaticFiles.hs -- Template Haskell for determining
									 -- which static files to include
									 -- using compile-time verificatoin
	/Application.hs -- Boilerplate code to make Yesod work
									-- as a web application. You likely
									-- will not need to touch this much.
	/Foundation.hs -- Core code for changing how the
								 -- application works. You will be editing
								 -- this.
	/Import.hs -- This file is imported everywhere
						 -- Any types/functions/values that need to be
						 -- accessible in most of your application should
						 -- be imported here as `import Module as Import`
	/Model.hs
	/Settings.hs
/static
/templates
/test
```

## Database Setup

After installing Postgres ([Linux installation](https://www.postgresql.org/download/linux/ubuntu/) / [Installation from source code](https://www.postgresql.org/docs/12/install-short.html), run:

```
# Switch to the `postgres` user
su postgres
# Type in password

# Create the user
createuser my-yesod --superuser --pwprompt
# Type in 'my-yesod' for the password
# Note: see `/config/settings.yml` to change the
# password Yesod will use to something else

# Create the live and test databases
createdb my-yesod
createdb my-yesod_test
```

## Development

Build the `yesod-bin` tool:
```bash
stack build --copy-compiler-tool yesod-bin
```

Start a development server:
```bash
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag my-yesod:library-only --flag my-yesod:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).
