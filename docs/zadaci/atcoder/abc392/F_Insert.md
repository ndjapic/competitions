# Задатак: F_Insert.pas

```pascal
program F_Insert;
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
        KeyAcc: Int64; // Keys accumulation of subtree rooted at this node
    end;

    TArrayTreap = class
    private
        FRoot: TNode;
        procedure UpdateNode(N: TNode);
        procedure Split(N: TNode; Index: Integer; var L, R: TNode); // Split tree N into two trees: L (< Index) and R (>= Index)
        procedure Merge(var T: TNode; L, R: TNode); // Merge trees L and R into tree T
        function NewNode(Key: Integer): TNode;
        procedure FreeNode(N: TNode);
        function FindByIndex(N: TNode; Index: Integer): TNode; // Find node with given index
        function GetIndex(N: TNode): Integer; // Get index of node N in the tree
        function GetCount(N: TNode): Integer;
        function GetCount: Integer; overload;
        function GetAcc(N: TNode): Integer;
        function GetAcc: Integer; overload;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Insert(Index: Integer; Value: Integer);
        procedure Delete(Index: Integer);
        function Get(Index: Integer): Integer;
        function Query(Index1, Index2: Integer): Int64;
        procedure Update(Index: Integer; Value: Integer);
        property Count: Integer read GetCount;
        property Acc: Integer read GetAcc;
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

function TArrayTreap.NewNode(Key: Integer): TNode;
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

function TArrayTreap.GetCount(N: TNode): Integer;
begin
    if N = nil then
        Result := 0
    else
        Result := N^.Size;
end;

function TArrayTreap.GetCount: Integer; overload;
begin
    Result := GetCount(FRoot);
end;

function TArrayTreap.GetAcc(N: TNode): Integer;
begin
    if N = nil then
        Result := 0
    else
        Result := N^.KeyAcc;
end;

function TArrayTreap.GetAcc: Integer; overload;
begin
    Result := GetAcc(FRoot);
end;

procedure TArrayTreap.UpdateNode(N: TNode);
begin
    N^.Size := GetCount(N^.Left) + GetCount(N^.Right) + 1; 
    N^.KeyAcc := GetAcc(N^.Left) + GetAcc(N^.Right) + N^.Key; 
end;

procedure TArrayTreap.Split(N: TNode; Index: Integer; var L, R: TNode);
var
    RIndex: Integer;
begin
    if N = nil then begin
        L := nil;
        R := nil;
    end else begin
        RIndex := Index - GetCount(N^.Left) - 1;

        if RIndex >= 0 then begin
            Split(N^.Right, RIndex, L, R);
            N^.Right := L;
            L := N;
        end else begin
            Split(N^.Left, Index, L, R);
            N^.Left := R;
            R := N;
        end;

        UpdateNode(N);
    end;
end;

procedure TArrayTreap.Merge(var T: TNode; L, R: TNode);
begin
    if L = nil then
        T := R
    else if R = nil then
        T := L
    else begin

        if L^.Priority > R^.Priority then begin
            Merge(L^.Right, L^.Right, R);
            T := L;
        end else begin
            Merge(R^.Left, L, R^.Left);
            T := R;
        end;

        UpdateNode(T);
    end;
end;

function TArrayTreap.GetIndex(N: TNode): Integer;
begin
    if N = nil then
        Result := 0
    else
        Result := GetCount(N^.Left);
end;

function TArrayTreap.FindByIndex(N: TNode; Index: Integer): TNode;
begin
    if N = nil then
        Result := nil
    else if Index < GetCount(N^.Left) then
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
    Split(FRoot, Index, L, R); // Split into left part (before Index) and right part (after Index)
    Merge(L, L, NewNode(Value)); // Create new node
    Merge(FRoot, L, R); // Merge left part, new node, and right part
end;

procedure TArrayTreap.Delete(Index: Integer);
var
    L, M, R: TNode;
begin
    if (0 <= Index) and (Index < Count) then begin
        Split(FRoot, Index, L, R); // Split into left part (before Index) and right part
        Split(R, 1, M, R); // Split middle node and right part
        FreeNode(M);
        Merge(FRoot, L, R); // Merge left part and right part
    end;
end;

function TArrayTreap.Get(Index: Integer): Integer;
begin
    if (Index < 0) or (Index >= Count) then
        Result := -1 // Or raise an exception
    else
        Result := FindByIndex(FRoot, Index)^.Key;
end;

function TArrayTreap.Query(Index1, Index2: Integer): Int64;
var
    L, M, R: TNode;
begin
    Split(FRoot, Index2, L, R); // Split into left part (before Index1) and right part
    Split(L, Index1, L, M); // Split into left part (before Index1) and middle part
    Result := GetAcc(M);
    Merge(L, L, M); // Merge left part and middle part
    Merge(FRoot, L, R); // Merge left part and right part
end;

procedure TArrayTreap.Update(Index: Integer; Value: Integer);
begin
    if (0 <= Index) and (Index < Count) then
        FindByIndex(FRoot, Index)^.Key := Value;
end;

var
    n, i, p: Integer;
    a: TArrayTreap;

begin
    readln(n);
    a := TArrayTreap.Create;

    for i := 1 to n do begin
        read(p);
        a.Insert(p-1, i);
    end;
    readln;

    for i := 0 to n-1 do begin
        write(a.Get(i));
        if i < n-1 then write(' ');
    end;
    writeln;
end.

```
