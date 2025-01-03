program DynamicArrayTreap;
{$MODE DELPHI}

{interface}

uses
    SysUtils;

type
    TPriority = LongInt;
    TNode = ^TTreeNode;
    TTreeNode = record
        Key: Integer; // Can be any comparable type
        Priority: TPriority;
        Left, Right: TNode;
        Size: Integer; // Size of subtree rooted at this node
    end;

    TArrayTreap = class
    private
        FRoot: TNode;
        procedure Split(N: TNode; Index: Integer; var L, R: TNode); // Split tree N into two trees: L (< Index) and R (>= Index)
        procedure Merge(var T: TNode; L, R: TNode); // Merge trees L and R into tree T
        function GetNode(Key: Integer): TNode;
        procedure FreeNode(N: TNode);
        function FindByIndex(N: TNode; Index: Integer): TNode; // Find node with given index
        function GetIndex(N: TNode): Integer; // Get index of node N in the tree
    public
        constructor Create;
        destructor Destroy; override;
        procedure Insert(Index: Integer; Value: Integer);
        procedure Delete(Index: Integer);
        function Get(Index: Integer): Integer;
        procedure Put(Index: Integer; Value: Integer);
    private
        function GetCount(N: TNode): Integer;
        function GetCount: Integer; overload;
    public
        property Count: Integer read GetCount;
    end;

{implementation}

constructor TArrayTreap.Create;
begin
    FRoot := nil;
end;

destructor TArrayTreap.Destroy;
begin
    FreeNode(FRoot);
end;

function TArrayTreap.GetNode(Key: Integer): TNode;
begin
    New(Result);
    Result^.Key := Key;
    Result^.Priority := Random(MaxInt); // Generate random priority
    Result^.Left := nil;
    Result^.Right := nil;
    Result^.Size := 1;
end;

procedure TArrayTreap.FreeNode(N: TNode);
begin
    if N <> nil then
    begin
        FreeNode(N^.Left);
        FreeNode(N^.Right);
        Dispose(N);
    end;
end;

procedure TArrayTreap.Split(N: TNode; Index: Integer; var L, R: TNode);
begin
    if N = nil then
    begin
        L := nil;
        R := nil;
        Exit;
    end;

    if GetCount(N^.Left) < Index then
    begin
        Split(N^.Right, Index - GetCount(N^.Left) - 1, L, R);
        N^.Right := L;
        L := N;
    end
    else
    begin
        Split(N^.Left, Index, L, R);
        N^.Left := R;
        R := N;
    end;

    N^.Size := GetCount(N^.Left) + GetCount(N^.Right) + 1; 
end;

procedure TArrayTreap.Merge(var T: TNode; L, R: TNode);
begin
    if L = nil then
    begin
        T := R;
        Exit;
    end;

    if R = nil then
    begin
        T := L;
        Exit;
    end;

    if L^.Priority > R^.Priority then
    begin
        Merge(L^.Right, L^.Right, R);
        T := L;
    end
    else
    begin
        Merge(R^.Left, L, R^.Left);
        T := R;
    end;

    T^.Size := GetCount(T^.Left) + GetCount(T^.Right) + 1; 
end;

function TArrayTreap.GetIndex(N: TNode): Integer;
begin
    if N = nil then
        Exit(0);

    Result := GetCount(N^.Left);
end;

function TArrayTreap.FindByIndex(N: TNode; Index: Integer): TNode;
begin
    if N = nil then
        Exit(nil);

    if Index < GetCount(N^.Left) then
        Result := FindByIndex(N^.Left, Index)
    else if Index > GetCount(N^.Left) then
        Result := FindByIndex(N^.Right, Index - GetCount(N^.Left) - 1)
    else
        Result := N;
end;

procedure TArrayTreap.Insert(Index: Integer; Value: Integer);
var
    L, R: TNode;
begin
    Split(FRoot, Index - 0, L, R); // Split into left part (before Index) and right part (after Index)
    Merge(L, L, GetNode(Value)); // Create new node
    Merge(FRoot, L, R); // Merge left part, new node, and right part
end;

procedure TArrayTreap.Delete(Index: Integer);
var
    L, M, R: TNode;
begin
    if (Index < 0) or (Index >= Count) then
        Exit;

    Split(FRoot, Index, L, R); // Split into left part (before Index) and middle node
    Split(R, 1, M, R); // Split middle node and right part
    FreeNode(M);
    Merge(FRoot, L, R); // Merge left part and right part
end;

function TArrayTreap.Get(Index: Integer): Integer;
begin
    if (Index < 0) or (Index >= Count) then
        Exit(-1); // Or raise an exception

    Result := FindByIndex(FRoot, Index)^.Key;
end;

procedure TArrayTreap.Put(Index: Integer; Value: Integer);
begin
    if (Index < 0) or (Index >= Count) then
        Exit; // Or raise an exception

    FindByIndex(FRoot, Index)^.Key := Value;
end;

function TArrayTreap.GetCount(N: TNode): Integer;
begin
    if N = nil then
        Exit(0);

    Result := N^.Size;
end;

function TArrayTreap.GetCount: Integer; overload;
begin
    Result := GetCount(FRoot);
end;

var
    MyArray: TArrayTreap;
    i: Integer;

begin
    MyArray := TArrayTreap.Create;

    MyArray.Insert(0, 0);
    MyArray.Insert(1, 1);
    MyArray.Insert(2, 4);
    MyArray.Insert(3, 9);
    MyArray.Insert(4, 16);

    for i := 0 to MyArray.Count do
        Writeln('Get(', i, '): ', MyArray.Get(i)); 
    MyArray.Put(2, -4);
    MyArray.Insert(5, -25);
    for i := 0 to MyArray.Count do
        Writeln('Get(', i, ') after Insert: ', MyArray.Get(i)); 
    MyArray.Delete(3);
    for i := 0 to MyArray.Count do
        Writeln('Get(', i, ') after Delete: ', MyArray.Get(i)); 

    MyArray.Free;
end.

(*
Get(0): 0
Get(1): 1
Get(2): 4
Get(3): 9
Get(4): 16
Get(5): -1
Get(0) after Insert: 0
Get(1) after Insert: 1
Get(2) after Insert: -4
Get(3) after Insert: 9
Get(4) after Insert: 16
Get(5) after Insert: -25
Get(6) after Insert: -1
Get(0) after Delete: 0
Get(1) after Delete: 1
Get(2) after Delete: -4
Get(3) after Delete: 16
Get(4) after Delete: -25
Get(5) after Delete: -1

------------------
(program exited with code: 0)
Press return to continue
*)
