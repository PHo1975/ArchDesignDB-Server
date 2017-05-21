# ArchDesignDB-Server
Server for ArchDesignDB for Java Swing Client and Web Client.
It includes a Jetty based Webserver to deliver the Web Apps.
The server is managed by a Java Swing based UI.

The package also contains the Java Swing Client.


## Data Model
Any data in the database is stored in an instance of a certain classtype.
Every classtype has an unique Integer ID and any Instance of that class has an unique Instance ID.
So every Instance can be adressed by a (classTypeID/InstanceID) adress.
Classes can contain a certain number of data fields and actions (=methods).
Basic data types for fields are: Int, Long, Double, UnitNumber, Currency, String, Date, Enum, Object reference, Field reference, Binary operation, function call, Collecting function call, Blob, (3-dimensional) Vector, (2-dimensional) Polygon.
Classes can inherit their capabilities from parent classes. Similar classes can be used in a polymorphic way.
The class structure can be designed in the class editor in the server UI.

## Database Structure
A class can declare that its instances can contain other instances in named subfolders, called "property fields".
For each property field it can be defined how many instances of what class can be inserted.
That way users can build up hierarchical project structures of any complexity.
Polymorphy can be used to allow a range of similar classes that stem from a common super class.
It is possible to automatically create instances of a certain type in property fields of a class at creation time of the parent instance.
The database API is optimized to load hierachical data as the index files already contain the parent-child relationships of all instances.

## Instance operations, Links
Instances can be moved and copied to other property fields of other parents, as long as they comply with the classtype restrictions in the target property field.
Additionally instances can be linked to another place, so in face a shadow copy is created. In fact the linked instance is only stored once in the database and used in multiple places. That way objects like adresses can be shared between different projects and stay up to date. A change in one place will lead to a change in the other place, too.

## Math, Functions, Collecting Functions
Any data field of an instance contains an _expression_. So its not only possible to insert a constant value, but also a math expression like 12.4 / 48.0. That expression is stored in the database and the result value can be further used. The math operations + - * / ^ , Brackets and the bool operations | and & are supported.
The database core contains a library with _math functions_ like min(), max(), sin(), cos() that can be used in expressions. 
There are also text operations like concat().
Additional _function libraries_ (as JVM class files) can be added to the server to allow custom operations.
Expression can also contain references to other fields, also in other instances. The fields get adressed by (ClassID, InstanceID, FieldNr).
As soon as the referenced field is changed during a transaction, the referencing expression is recalculated. This also works in cascades.
The server checks on input that circular references are avoided.
_Collecting functions_ in an expression are applied on all child instances of a certain property field (sub folder). So a certain field (=column) of all child instances can e.g. be summed up, or you can find the min/max values. These calculations are automatically updated after each change of the child instances.

## Multiuser collaboration, screen updates
The database is designed for collaboration in small workgroups (3-4 users). Transaction locking works per-instance. So its possible to work simultaneously on the same data.
To show any data on screen, the Java and Web clients subscribe to the requested data and get callback notification of any data change.
That way the editing user gets a validation that his data are stored correctly, and another user watching the same data on another client gets instant updates on screen.

## Undo
At the moment transaction Undos can only be done globally, so the very last action will be revoked, no matter what user did it. When trying to Undo, you see a list of last activities and the users who did them, so you can coordinate in your team if the Undo is possible. The undo feature is designed to revoke harmfull changes in the database, e.g. accidentally deleting data.

## Drawing module
The database system contains a simple 2D drawing module focused on architectural CAD. Drawing objects like lines, arcs, ellipses, polygons and dimensions can be created and modified using the actions defined in their classes.
The actions are implemented in plug-in classes that are referenced in the class description.
At the moment the full functionality is only included in the Java Swing client. The Web Client just contains a basic WebGL-Viewer.

## Configuration
The basic configuration is stored in Java preferences / user node. On Windows the server must have the rights to access the Window registry.
The local "config" directory contains the following files:
- types.xml to store the type configuration of all classes
- users.xml to store the credentials of all registered users
- for each user there are settings files to store the configuration of their clients
- all other system settings (currency, dimensions, holidays, plug-ins, ...) are stored inside the database in the "settings" node below the "root" node

## Storage
In the local "data" directory the folling files are stored:
- for each class type an index file
- blob files for instance data, inheritage, external links and collecting functions
- transaction log, transaction detail log, usage statistics
Any change in the database is done in a atomic transaction. New or changed data is simply added to the blob files and the indexes updated to the new data position. Any change of the index files is logged in the transaction logs. They can be used to undo the last transactions.
The server creates a full backup of all data files in a fixed, adjustable period (e.g. once per day). The backups of the last 2 days are kept, too.
