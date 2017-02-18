unit BeaRLibItems;

interface

type
  TItem = record
    ItemID: Integer;
    X: Integer;
    Y: Integer;
    MapID: Integer;
  end;

type
  TItems = array of TItem;   

// Library
procedure Items_Open(); stdcall; external 'BeaRLibItems.dll';
procedure Items_Close(); stdcall; external 'BeaRLibItems.dll';
function Items_GetVersion(): PChar; stdcall; external 'BeaRLibItems.dll';
// Map
procedure Items_Ground_Clear(); stdcall; external 'BeaRLibItems.dll';
procedure Items_Ground_MapClear(MapID: Integer); stdcall; external 'BeaRLibItems.dll';
function Items_Ground_MapClearXY(MapID: Integer; AX, AY: Integer; var AItems: TItems): Boolean; stdcall; external 'BeaRLibItems.dll';

function Items_Ground_GetCount(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Ground_GetMapCount(MapID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Ground_GetMapCountXY(MapID: Integer; AX, AY: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Ground_SetItem(Index: Integer; AItem: TItem): Boolean; stdcall; external 'BeaRLibItems.dll';
function Items_Ground_GetItem(Index: Integer): TItem; stdcall; external 'BeaRLibItems.dll';

procedure Items_Ground_SetItems(var AItems: TItems); stdcall; external 'BeaRLibItems.dll';
procedure Items_Ground_GetItems(var AItems: TItems); stdcall; external 'BeaRLibItems.dll';
procedure Items_Ground_GetMapItems(MapID: Integer; var AItems: TItems); stdcall; external 'BeaRLibItems.dll';
procedure Items_Ground_GetMapItemsXY(MapID: Integer; AX, AY: Integer; var AItems: TItems); stdcall; external 'BeaRLibItems.dll';

procedure Items_Ground_Items_Append(AItem: TItem); stdcall; external 'BeaRLibItems.dll';
function Items_Ground_Items_Delete(Index: Integer; var AItem: TItem): Boolean; stdcall; external 'BeaRLibItems.dll';
function Items_Ground_Items_DeleteXY(MapID: Integer; Index, AX, AY: Integer; var AItem: TItem): Boolean; stdcall; external 'BeaRLibItems.dll';

// Inventory
procedure Items_Inventory_Clear(); stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetCount(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_SetItem(Index: Integer; AItem: TItem): Boolean; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetItem(Index: Integer): TItem; stdcall; external 'BeaRLibItems.dll';
procedure Items_Inventory_GetItems(var AItems: TItems); stdcall; external 'BeaRLibItems.dll';
procedure Items_Inventory_SetItems(var AItems: TItems); stdcall; external 'BeaRLibItems.dll';
procedure Items_Inventory_Items_Append(AItem: TItem); stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_Items_Delete(Index: Integer; var AItem: TItem): Boolean; stdcall; external 'BeaRLibItems.dll';

implementation

end.
