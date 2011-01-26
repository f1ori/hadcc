
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

dojo.addOnLoad(function(){
    var nicklistGrid = dojo.byId("nicklistGrid");
    // doesn't work, why???
    //dojo.connect(nicklistGrid, "onRowDblClick", newFilelistTab);
});

// vim: sw=4 ai expandtab
