# config_service

The config service provides a very simple data model for modeling
configuration using a DAG.

Each node in the DAG has a collection of k/v pairs:

    ux
      * THEME="default"
      * API_ENDPOINT = "http://core.gannett.com/v1/"

    usat
      * THEME="usat"
      * TWITTER_ACCOUNT=usatoday

    uscp
      * THEME="uscp"

    broadcast
      * THEME="broadcast"

    indy
      * THEME="indy"
      * API_ENDPOINT = "http://core.gannet.com/v2/"

A path in the graph is encoded in a CSV:

 * ux,usat
 * ux,broadcast,wbir
 * ux,broadcast,indy
 
Key/Value pairs in the graph are merged into a single value where the
child on the right overrides the parent on the left.  

For instance the path `ux,broadcast,wbir` is materialized as:

    THEME="broadcast" # from the broadcast node
    API_ENDPOINT="http://core.gannett.com/v1/" # from the ux node
    

However, let us say that `indy` is testing v2 of the core API, the
config path: `ux,uscp,indy` materializes as:

    THEME="indy" # from the indy node
    API_ENDPOINT="http://core.gannett.com/v1/" # from the indy node
    

