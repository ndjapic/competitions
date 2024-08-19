program SortedTreap;

type
    TSortedTreap<_T> = class
        Left, Right: TSortedTreap;
        Key: _T;
        Priority: Integer;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Integer; static; abstract;
        class function GetSize(Other: TSortedTreap): Integer; static;
        constructor Create(K: _T);
        destructor Destroy; override;
        procedure Update;
        procedure Split(K: _T; var L, R: TSortedTreap); (* Needs implementation *)
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap); (* Needs implementation *)
        procedure Merge(L, R: TSortedTreap); (* Needs implementation *)
        procedure Insort(K: _T); (* Needs implementation *)
        procedure Delete(K: _T);
        // Other methods as needed
        property At[i: Integer]: _T Read GetAt; default;
    end;
    TIntTreap = TSortedTreap<Integer>;

class function TIntTreap.Compare(lhs, rhs: Integer): Integer;
begin
    Result := lhs - rhs;
end;

constructor TSortedTreap.Create(K: _T);
begin
    Inherited Create;
    Key := K;
    Priority := Random(MaxInt); // Generate random priority
    Size := 1;
    Left := nil;
    Right := nil;
end;

destructor TSortedTreap.Destroy;
begin
    if Left <> nil then Left.Free;
    if Right <> nil then Right.Free;
    Self.Free; (* Free current node *)
end;

class function TSortedTreap.GetSize(Other: TSortedTreap): Integer;
begin
    if Other = nil then
        Result := 0
    else
        Result := Other.Size;
end;

procedure TSortedTreap.Update;
begin
    Size := GetSize(Left) + 1 + GetSize(Right);
end;

procedure TSortedTreap.Split(K: _T; var L, R: TSortedTreap);
begin
    if Compare(K, Key) < 0 then begin
        if Left = nil then
            L := nil
        else
            Left.Split(K, L, Left);
        R := Self;
    end else begin
        if Right = nil then
            R := nil
        else
            Right.Split(K, Right, R);
        L := Self;
    end;
    Update;
end;

function TSortedTreap.Rank(K: _T): Integer;
var
    L, R: TSortedTreap;
begin
    Split(K, L, R);

    if L = nil then
        Result := 0;
    else begin
        Result := L.Size;
        Merge(L, R);
    end;
end;

procedure TSortedTreap.Delete(K: _T);
var
    L, M, R: TSortedTreap;
begin
    Split(K, L, R);

    if R <> nil then begin
        R.SplitByRank(1, M, R);
        if M.Key = K then begin
            M.Free
        else if R = nil then
            R := M
        else
            R.Merge(M, R);
    end;

    Merge(L, R);
end;

var
    n, i, K: Integer;
    a: TIntTreap;

begin
    a := nil;
    ReadLn(n);

    for i := 0 to n-1 do begin
        Read(K);
        if a = nil then
            a := TIntTreap.Create(K)
        else
            a.Insort(K);
    end;
    ReadLn;

    a.Delete(9); // Delete one of two nines.
    a.Delete(10); // Does nothing.
    n := a.Size; // Update n.

    for i := 0 to n-2 do Write(a[i], ' ');
    WriteLn(a[n-1]);
    a.Free;
end.

(*
* Input
* 23
* 2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64
* *)
