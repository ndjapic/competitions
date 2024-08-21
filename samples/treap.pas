program SortedTreap;
{$mode delphi}{$inline on}
uses
    sysutils;

type
    TSortedTreap<_T> = class
        Left, Right: TSortedTreap<_T>;
        Key: _T;
        Priority: Integer;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Integer; inline; static;
        class function GetSize(Other: TSortedTreap<_T>): Integer; static;
        procedure Update;
        constructor Create(K: _T);
        destructor Destroy; override;
        procedure Split(K: _T; var L, R: TSortedTreap<_T>);
        constructor Merge(L, R: TSortedTreap<_T>);
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap<_T>); (* Needs implementation *)
        constructor Insort(K: _T);
        procedure Delete(K: _T);
        procedure dfs;
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
    // FreeAndNil(Self);
    Inherited;
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

constructor TSortedTreap<_T>.Merge(L, R: TSortedTreap<_T>);
begin
    if L = nil then
        Self := R
    else if R = nil then
        Self := L
    else begin
        if L.Priority < R.Priority then begin
            Self := R;
            Left := L.Merge(L, Left);
        end else begin
            Self := L;
            Right := R.Merge(Right, R);
        end;
        Update;
    end;
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
        Self := TSortedTreap<_T>.Merge(L, R);
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

constructor TSortedTreap<_T>.Insort(K: _T);
var
    L, R: TSortedTreap<_T>;
begin
    Write('Entering Insort ', K, ': '); dfs; WriteLn;
    Split(K, L, R);
    Write('After Split: '); dfs; WriteLn;
    Self := TSortedTreap<_T>.Create(K);
    Self := TSortedTreap<_T>.Merge(L, Self);
    Write('Between Merge: '); dfs; WriteLn;
    Self := TSortedTreap<_T>.Merge(Self, R);
    Write('Exiting Insort ', K, ': '); dfs; WriteLn;
end;

procedure TSortedTreap<_T>.Delete(K: _T);
var
    L, M, R: TSortedTreap<_T>;
begin
    Split(K, L, R);

    if R <> nil then begin
        R.SplitByRank(1, M, R);
        if M.Key = K then
            FreeAndNil(M)
        else if R = nil then
            R := M
        else
            R := TSortedTreap<_T>.Merge(M, R);
    end;

    Self := TSortedTreap<_T>.Merge(L, R);
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
    n, i, K: Integer;
    a: TIntTreap;

begin
    Randomize;
    a := nil;
    ReadLn(n);

    for i := 0 to n-1 do begin
        Read(K);
        if a = nil then
            a := TIntTreap.Create(K)
        else
            a := a.Insort(K);
        {if i < 2 then a.dfs; WriteLn;}
    end;
    ReadLn;

    {a.Delete(9); // Delete one of two nines.
    a.Delete(10); // Does nothing.}
    n := a.Size; // Update n.

    for i := 0 to n-2 do Write(a[i], ' ');
    WriteLn(a[n-1]);
    {a.Free;}
end.

(*
Input
23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	0
Exec Time	0 ms
Memory	1628 KB

Standard Output
Entering Insort 3: [2]
After Split: [2]
Between Merge: [2[3]]
Exiting Insort 3: [2[3]]
Entering Insort 4: [2[3]]
After Split: [2[3]]
Between Merge: [2[3[4]]]
Exiting Insort 4: [2[3[4]]]
Entering Insort 5: [2[3[4]]]
After Split: [2[3[4]]]
Between Merge: [[2[3[4]]]5]
Exiting Insort 5: [[2[3[4]]]5]
Entering Insort 6: [[2[3[4]]]5]
After Split: [[2[3[4]]]5]
Between Merge: [[2[3[4]]]5[6]]
Exiting Insort 6: [[2[3[4]]]5[6]]
Entering Insort 7: [[2[3[4]]]5[6]]
After Split: [[2[3[4]]]5[6]]
Between Merge: [[2[3[4]]]5[[6]7]]
Exiting Insort 7: [[2[3[4]]]5[[6]7]]
Entering Insort 8: [[2[3[4]]]5[[6]7]]
After Split: [[2[3[4]]]5[[6]7]]
Between Merge: [[2[3[4]]]5[[[6]7]8]]
Exiting Insort 8: [[2[3[4]]]5[[[6]7]8]]
Entering Insort 9: [[2[3[4]]]5[[[6]7]8]]
After Split: [[2[3[4]]]5[[[6]7]8]]
Between Merge: [[2[3[4]]]5[[[[6]7]8]9]]
Exiting Insort 9: [[2[3[4]]]5[[[[6]7]8]9]]
Entering Insort 4: [[2[3[4]]]5[[[[6]7]8]9]]
After Split: [[4]5[[[[6]7]8]9]]
Between Merge: [2[[3]4]]
Exiting Insort 4: [[2[[3]4[4]]]5[[[[6]7]8]9]]
Entering Insort 9: [[2[[3]4[4]]]5[[[[6]7]8]9]]
After Split: [[2[[3]4[4]]]5[[[6]7]8]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9]
Exiting Insort 9: [[[2[[3]4[4]]]5[[[6]7]8]]9[9]]
Entering Insort 16: [[[2[[3]4[4]]]5[[[6]7]8]]9[9]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[9]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16]]
Exiting Insort 16: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16]]
Entering Insort 25: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25]]]
Exiting Insort 25: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25]]]
Entering Insort 36: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25]]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25]]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25[36]]]]
Exiting Insort 36: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25[36]]]]
Entering Insort 49: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25[36]]]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[[9]16[25[36]]]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49]]
Exiting Insort 49: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49]]
Entering Insort 64: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49[64]]]
Exiting Insort 64: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49[64]]]
Entering Insort 81: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49[64]]]
After Split: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[9]16[25[36]]]49[64]]]
Between Merge: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[[9]16[25[36]]]49[64]]81]]
Exiting Insort 81: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[[9]16[25[36]]]49[64]]81]]
Entering Insort 8: [[[2[[3]4[4]]]5[[[6]7]8]]9[[[[9]16[25[36]]]49[64]]81]]
After Split: [[8]9[[[[9]16[25[36]]]49[64]]81]]
Between Merge: [[[2[[3]4[4]]]5[[6]7]]8]
Exiting Insort 8: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[25[36]]]49[64]]81]]
Entering Insort 27: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[25[36]]]49[64]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[9]16[25]]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[9]16[[25]27]]]
Exiting Insort 27: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[[25]27[36]]]49[64]]81]]
Entering Insort 64: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[[25]27[36]]]49[64]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9]16[[25]27[36]]]49]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9]16[[25]27[36]]]49[64]]]
Exiting Insort 64: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[[25]27[36]]]49[64[64]]]81]]
Entering Insort 16: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9]16[[25]27[36]]]49[64[64]]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[9]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[9[16]]]
Exiting Insort 16: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[36]]]49[64[64]]]81]]
Entering Insort 81: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[36]]]49[64[64]]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9[16]]16[[25]27[36]]]49[64[64]]]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9[16]]16[[25]27[36]]]49[[64[64]]81]]]
Exiting Insort 81: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[36]]]49[[64[64]]81]]81]]
Entering Insort 32: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[36]]]49[[64[64]]81]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[9[16]]16[[25]27]]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[9[16]]16[[25]27[32]]]]
Exiting Insort 32: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[32[36]]]]49[[64[64]]81]]81]]
Entering Insort 64: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[32[36]]]]49[[64[64]]81]]81]]
After Split: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9[16]]16[[25]27[32[36]]]]49]]
Between Merge: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[9[16]]16[[25]27[32[36]]]]49[64]]]
Exiting Insort 64: [[[[2[[3]4[4]]]5[[6]7]]8[8]]9[[[[9[16]]16[[25]27[32[36]]]]49[[64[64[64]]]81]]81]]
2 3 4 4 5 6 7 8 8 9 9 16 16 25 27 32 36 49 64 64 64 81 81
*)
