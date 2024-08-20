program SortedTreap;

type
    TSortedTreap<_T> = class
        Left, Right: TSortedTreap<_T>;
        Key: _T;
        Priority: Integer;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Integer; virtual;
        class function GetSize(Other: TSortedTreap<_T>): Integer; static;
        procedure Update;
        constructor Create(K: _T);
        destructor Destroy; override;
        procedure Split(K: _T; var L, R: TSortedTreap<_T>);
        procedure Merge(L, R: TSortedTreap<_T>);
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap<_T>); (* Needs implementation *)
        procedure Insort(K: _T);
        procedure Delete(K: _T);
        function GetAt(i: Integer): _T;
        property At[i: Integer]: _T Read GetAt; default;
    end;
    TIntTreap = TSortedTreap<Integer>;

(* BEGIN TSortedTreap *)

class function TSortedTreap<_T>.Compare(lhs, rhs: _T): Integer;
begin
    Result := lhs - rhs;
end;

class function TSortedTreap<_T>.GetSize(Other: TSortedTreap<_T>): Integer;
begin
    if Other = nil then
        Result := 0
    else
        Result := Other.Size;
end;

procedure TSortedTreap<_T>.Update;
begin
    Size := GetSize(Left) + 1 + GetSize(Right);
end;

constructor TSortedTreap<_T>.Create(K: _T);
begin
    Inherited Create;
    Key := K;
    Priority := Random(MaxInt); // Generate random priority
    Size := 1;
    Left := nil;
    Right := nil;
end;

destructor TSortedTreap<_T>.Destroy;
begin
    if Left <> nil then Left.Free;
    if Right <> nil then Right.Free;
    Free;
end;

procedure TSortedTreap<_T>.Split(K: _T; var L, R: TSortedTreap<_T>);
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

procedure TSortedTreap<_T>.Merge(L, R: TSortedTreap<_T>);
begin
    if L = nil then
        Self := R
    else if R = nil then
        Self := L
    else if L.Priority < R.Priority then begin
        Self := R;
        Left.Merge(L, Left);
    end else begin
        Self := L;
        Right.Merge(Right, R);
    end;
    if Self <> nil then Update;
end;

function TSortedTreap<_T>.Rank(K: _T): Integer;
var
    L, R: TSortedTreap<_T>;
begin
    Split(K, L, R);

    if L = nil then
        Result := 0
    else begin
        Result := L.Size;
        Merge(L, R);
    end;
end;

procedure TSortedTreap<_T>.SplitByRank(i: Integer; var L, R: TSortedTreap<_T>);
var
    j: Integer;
begin
    j := i - GetSize(Left) - 1;
    if j < 0 then begin
        if Left = nil then
            L := nil
        else
            Left.SplitByRank(i, L, Left);
        R := Self;
    end else begin
        if Right = nil then
            R := nil
        else
            Right.SplitByRank(j, Right, R);
        L := Self;
    end;
    Update;
end;

procedure TSortedTreap<_T>.Insort(K: _T);
var
    L, M, R: TSortedTreap<_T>;
begin
    Split(K, L, R);
    M := TSortedTreap<_T>.Create(K);
    if R = nil then
        L.Merge(L, M)
    else
        R.Merge(M, R);
    Merge(L, R);
end;

procedure TSortedTreap<_T>.Delete(K: _T);
var
    L, M, R: TSortedTreap<_T>;
begin
    Split(K, L, R);

    if R <> nil then begin
        R.SplitByRank(1, M, R);
        if M.Key = K then begin
            M.Free;
            M := nil;
        end else if R = nil then
            R := M
        else
            R.Merge(M, R);
    end;

    Merge(L, R);
end;

function TSortedTreap<_T>.GetAt(i: Integer): _T;
var
    j: Integer;
begin
    j := i - GetSize(Left) - 1;
    if (j < -1) and (Left <> nil) then
        Result := Left.GetAt(i)
    else if (j > -1) and (Right <> nil) then
        Result := Right.GetAt(j)
    else
        Result := Key;
end;

(* END TSortedTreap *)

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
Input
23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	139
Exec Time	544 ms
Memory	1050860 KB

"When a container exits with status code 139, it's because it received a SIGSEGV signal.
The operating system terminated the container's process to guard against a memory integrity violation.
It's important to investigate what's causing the segmentation errors
if your containers are terminating with code 139."
*)
