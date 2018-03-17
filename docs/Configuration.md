# Configuration

Especially for a future development we prepare a configuration through a configuration file. Because Haskell/Scotty does not provided any prepared mechanism for it we had to implemented it by my own. Currently there are 2 files - `app-config.cfg` and `build-info.cfg` which contain all information needed by a running application. For tests there exist special versions suffixed by `-test`. A format of configuration files is an old-style `Windows .INI` format.

An advantage of an external configuration is that the configuration do not have to be present during a compilation process and it can be attached later (during a start of the application).

## Build Configuration

A build configuration (`build-info.cfg` file) contains information about the last build of the application. This configuration can be created simply by running a prepared script `build-info.sh`. Normally this script is run by a CI Tool (Travis CI) during build process.

### Example
```
name = Data Stewardship Wizard Server
version = 1.0.0
builtat = 2017/10/25 19:50:20Z
```

## Application Configuration

Application Configuration (`app-config.cfg` file) contains 4 sections - `Web`, `Database`, `JWT` and `Role`. Currently there are not many things which can be configured but it is assumed that it will grow in the future.

### Sections

#### Web
In `Web` section we can configure on which port the server will be running.

#### Database

In `Database` section we can set up connection properties to our database.

#### JWT
`JWT` section contains property which holds a value of secret which is need in a process of signing JWT payload.

#### Role

In `Role` section we can assign permissions to roles. These sets of permissions will be used as templates for new users in a way that when a new user is created with some role he will get assigned a list of permissions based on this template from the configuration file.


### Example
```
[Web]
port = 3000

[Database]
host = mongo
dbname = dsw-server
port = 27017

[JWT]
secret = secret-key

[Role]
defaultrole = DATASTEWARD
admin = UM_PERM, ORG_PERM, KM_PERM, KM_UPGRADE_PERM, KM_PUBLISH_PERM, PM_PERM, WIZ_PERM, DMP_PERM
datasteward = KM_PERM, KM_UPGRADE_PERM, KM_PUBLISH_PERM, WIZ_PERM, DMP_PERM
researcher = WIZ_PERM, DMP_PERM

[Mail]
name = "DSW Wizard"
email = "info@dmp.fairdata.solutions"
host = "dmp.fairdata.solutions"
username = "knaisvoj"
password = "weewowfag42-"
```
