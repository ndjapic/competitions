program G_Edit_to_Match;
{$mode delphi}{$inline on}
uses
    math;
const
    nn = 200 * 1000;

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

var
    n, k, i, j, r, mn, ans: int32;
    s: array [0 .. nn] of string;
    a: TIntTreap;

(* BEGIN TSortedTreap *)

class function TSortedTreap<_T>.Compare(lhs, rhs: _T): Int32;
var
    j, mn: int32;
begin
    mn := min( length(s[lhs]), length(s[rhs]) );
    j := 1;
    while (j <= mn) and (s[lhs][j] = s[rhs][j]) do inc(j);

    if j <= mn then
        Result := ord(s[lhs][j]) - ord(s[rhs][j])
    else if j <= length(s[lhs]) then
        Result := 1
    else if j <= length(s[rhs]) then
        Result := -1
    else
        Result := 0;
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

begin
    readln(n);
    Randomize;
    a := nil;

    for k := 1 to n do begin
        readln(s[k]);
        ans := length(s[k]);
        TIntTreap.Insort(a, k);
        r := a.Rank(k);
        {Write('After Insort ', K, ': '); a.dfs; WriteLn;}

        if r > 0 then begin
            i := a[r-1];
            mn := min(length(s[k]), length(s[i]));
            setlength(s[0], mn);
            j := 1;
            while (j <= mn) and (s[k][j] = s[i][j]) do begin
                s[0][j] := s[k][j];
                inc(j);
            end;
            dec(j);
            setlength(s[0], j);

            ans := min(ans, length(s[k]) + length(s[i]) - (j-1)*2);
            {writeln(' r=',r, ' i=',i, ' j-1=',j-1, ' ans=',ans);}
        end;

        if r < k-1 then begin
            i := a[r+1];
            mn := min(length(s[k]), length(s[i]));
            j := 1;
            while (j <= mn) and (s[k][j] = s[i][j]) do inc(j);
            ans := min(ans, length(s[k]) + length(s[i]) - (j-1)*2);
            {writeln(' r=',r, ' i=',i, ' j-1=',j-1, ' ans=',ans);}
        end;

        writeln(ans);
    end;

    a.Free;
end.
