program D_Cross_Explosion;
{$mode delphi}{$inline on}
uses
    math;
const
    hh = 400 * 1000;

type
    TSortedTreap<_T> = class
        Left, Right: TSortedTreap<_T>;
        Key: _T;
        Priority: Integer;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Int32; inline; static;
        class function GetSize(Other: TSortedTreap<_T>): Integer; inline; static;
        procedure Update; inline;
        constructor Create(K: _T);
        destructor Destroy; override;
        procedure Split(K: _T; var L, R: TSortedTreap<_T>);
        class function Merge(var L, R: TSortedTreap<_T>): TSortedTreap<_T>; static;
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap<_T>);
        class procedure Insort(var M: TSortedTreap<_T>; K: _T); static;
        class procedure Discard(var M: TSortedTreap<_T>; K: _T); static;
        procedure dfs;
        function GetAt(i: Integer): _T;
        property At[i: Integer]: _T Read GetAt; default;
    end;
    TIntTreap = TSortedTreap<Int32>;

(* BEGIN TSortedTreap *)

class function TSortedTreap<_T>.Compare(lhs, rhs: _T): Int32;
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
    Priority := Random(High(Integer));
    Size := 1;
    Left := nil;
    Right := nil;
end;

destructor TSortedTreap<_T>.Destroy;
begin
    if Left <> nil then Left.Free;
    if Right <> nil then Right.Free;
    Inherited;
    Self := nil;
end;

procedure TSortedTreap<_T>.Split(K: _T; var L, R: TSortedTreap<_T>);
begin
    if Compare(Key, K) < 0 then begin
        if Right = nil then
            R := nil
        else
            Right.Split(K, Right, R);
        L := Self;
    end else begin
        if Left = nil then
            L := nil
        else
            Left.Split(K, L, Left);
        R := Self;
    end;
    Update;
end;

class function TSortedTreap<_T>.Merge(var L, R: TSortedTreap<_T>): TSortedTreap<_T>;
begin
    if (L = nil) or (R <> nil) and (L.Priority < R.Priority) then begin
        if R <> nil then begin
            R.Left := Merge(L, R.Left);
            R.Update;
        end;
        Result := R;
    end else begin
        L.Right := Merge(L.Right, R);
        L.Update;
        Result := L;
    end;
end;

function TSortedTreap<_T>.Rank(K: _T): Integer;
var
    L, R: TSortedTreap<_T>;
begin
    Split(K, L, R);
    Result := GetSize(L);
    Self := Merge(L, R);
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

class procedure TSortedTreap<_T>.Insort(var M: TSortedTreap<_T>; K: _T);
var
    L, R: TSortedTreap<_T>;
begin
    if M = nil then begin
        M := TSortedTreap<_T>.Create(K);
    end else begin
        M.Split(K, L, R);
        M := TSortedTreap<_T>.Create(K);
        M := Merge(L, M);
        M := Merge(M, R);
    end;
end;

class procedure TSortedTreap<_T>.Discard(var M: TSortedTreap<_T>; K: _T);
var
    L, R: TSortedTreap<_T>;
begin
    M.Split(K, L, R);

    if R <> nil then begin
        R.SplitByRank(1, M, R);
        if Compare(M.Key, K) = 0 then begin
            M.Free;
            M := nil;
        end else if R = nil then
            R := M
        else
            R := Merge(M, R);
    end;

    M := Merge(L, R);
end;

function TSortedTreap<_T>.GetAt(i: Integer): _T;
var
    j: Integer;
begin
    j := i - GetSize(Left) - 1;
    if (j < -1) {and (Left <> nil)} then
        Result := Left.GetAt(i)
    else if (j > -1) {and (Right <> nil)} then
        Result := Right.GetAt(j)
    else
        Result := Key;
end;

procedure TSortedTreap<_T>.dfs;
begin
    write('[');
    if Left <> nil then Left.dfs;
    write(Key);
    if Right <> nil then Right.dfs;
    write(']');
end;

(* END TSortedTreap *)

var
    h, w, q, k, r, c, i, walls: int32;
    hor, ver: array [1 .. hh] of TIntTreap;

procedure DiscardWall(r, c: int32);
begin
    TIntTreap.Discard(hor[r], c);
    TIntTreap.Discard(ver[c], r);
    dec(walls);
end;

begin
    randomize;
    readln(h, w, q);
    walls := h*w;

    for r := 1 to h do begin
        hor[r] := nil;
        for c := 1 to w do TIntTreap.Insort(hor[r], c);
    end;

    for c := 1 to w do begin
        ver[c] := nil;
        for r := 1 to h do TIntTreap.Insort(ver[c], r);
    end;

    for k := 1 to q do begin

        readln(r, c);

        if (hor[r] <> nil) and (hor[r].Rank(c) < hor[r].Rank(c+1)) then
            DiscardWall(r, c)
        else begin

            if hor[r] <> nil then begin
                i := hor[r].Rank(c);
                if i < hor[r].Size then DiscardWall(r, hor[r][i]);
                if i > 0 then DiscardWall(r, hor[r][i-1]);
            end;

            if ver[c] <> nil then begin
                i := ver[c].Rank(r);
                if i < ver[c].Size then DiscardWall(ver[c][i], c);
                if i > 0 then DiscardWall(ver[c][i-1], c);
            end;

        end;

    end;
    writeln(walls);
end.
