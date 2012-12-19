Usecases
========

Adding User
-----------
Data: <user_id> <password>

1. System checks if id is available
2. System hashes password so it is not retrievable
3. System saves user

Adding Site
-----------
Data: <user_id> <url>

1. System checks if url retrieves data
2. System adds site to user

Get News
--------
Data: <user_id>

1. System checks all sites of user for changes
2. When a site changed the user will be notified


Configure Site
--------------
Data: <user_id> <url> <configuration_data>

1. System creates configuration of user for this site

Configure User
--------------
Data: <user_id> <configuration_data>

1. System creates default configuration for user

Reset Site Configuration
------------------------
Data: <user_id> <url>

1. System deactivates site configuration

Reset User Configuration
------------------------
Data: <user_id>

1. System deactivates user configuration
