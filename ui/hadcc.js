
var newFilelistTab = function(e) {
    //var nicklistGrid = dojo.byId("nicklistGrid");
    var row = nicklistGrid.getItem(e.rowIndex);
    var nick = nicklistGrid.store.getIdentity(row);
    var filelistStore = new dojo.data.ItemFileReadStore({ url: '/filelist/'+escape(nick) });

    // set the layout structure:
    var filelistLayout = [{
        field: 'name',
        name: 'Name',
        width: '300px'
    },
    {
        field: 'size',
        name: 'Size',
        width: '50px'
    },
    {
        field: 'tth',
        name: 'TTH',
        width: 'auto'
    }];
    var filelistGrid = new dojox.grid.DataGrid({
        query: { 'parent': '0'},
        store: filelistStore,
        clientSort: true,
        structure: filelistLayout,
        region: 'center',
        splitter: true,
    });

    var filelistModel = new dijit.tree.TreeStoreModel({
        store: filelistStore,
        query: {id: '0'},
        //rootLabel: nick,
        //childrenAttrs: ["children"]
    });
    var dirTree = new dijit.Tree ({
        model: filelistModel,
        region: 'left',
        splitter: true,
        style: {width: '200px'},
        onClick: function(item) {
            var dir = filelistStore.getValue(item, 'id');
            console.log(dir);
            filelistGrid.setQuery({'parent': dir});
        },
    });

    var borderContainer = new dijit.layout.BorderContainer({
                  title: nick,
                  closable: true,
                  //gutters: true,
                  //livesplitter: true,
    });
    borderContainer.addChild(dirTree);
    borderContainer.addChild(filelistGrid);
    tabContainer.addChild(borderContainer);
    tabContainer.selectChild(borderContainer);
    filelistGrid.startup();
}

function eventHandler(lastId) {
    dojo.xhrGet( {
        url: "/events/"+lastId, 
        handleAs: "json",

        load: function(response, ioArgs) {
            if (response == null)
                return null;
            for (var i = 0; i < response.events.length; i++) {
                var e = response.events[i];
                if(e.type == "chatmsg") {
                    var lines = e.msg.split("\n");
                    var pane = dojo.byId("chatPane");
                    for (var j = 0; j < lines.length; j++) {
                        var line = lines[j];
                        pane.appendChild(document.createElement("div")).
                            appendChild(document.createTextNode(line));
                    }
                    pane.scrollTop = pane.scrollHeight;
                }
            }
            window.setTimeout("eventHandler("+response.newId+");", 500);
            return response;
        },

        error: function(response, ioArgs) {
            console.error("HTTP status code: ", ioArgs.xhr.status);
            return response;
        }
    });
}

dojo.addOnLoad(function(){
    var nicklistGrid = dojo.byId("nicklistGrid");
    // doesn't work, why???
    //dojo.connect(nicklistGrid, "onRowDblClick", newFilelistTab);
    dojo.byId("chatPane").
    appendChild(document.createElement("div")).
    appendChild(document.createTextNode("blabla"));
    eventHandler(0);
});

// vim: sw=4 ai expandtab
