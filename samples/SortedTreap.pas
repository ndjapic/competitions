program SortedTreap;
{$mode delphi}{$inline on}

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
    n, i, K: Integer;
    a: TIntTreap;

begin
    Randomize;
    a := nil;
    ReadLn(n);

    for i := 0 to n-1 do begin
        Read(K);
        TIntTreap.Insort(a, K);
        Write('After Insort ', K, ': '); a.dfs; WriteLn;
    end;
    ReadLn;

    TIntTreap.Discard(a, 9); // Delete one of two nines.
    TIntTreap.Discard(a, 10); // Does nothing.
    n := a.Size; // Update n.

    for i := 0 to n-1 do begin
      Write('Before Discard ', a[0], ': '); a.dfs; WriteLn;
      TIntTreap.Discard(a, a[0]);
    end;
    a.Free;
end.

(*
Standard Input

23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	0
Exec Time	0 ms
Memory	1504 KB

Standard Output

After Insort 2: [2]
After Insort 3: [[2]3]
After Insort 4: [[[2]3]4]
After Insort 5: [[[2]3]4[5]]
After Insort 6: [[[2]3]4[5[6]]]
After Insort 7: [[[2]3]4[5[[6]7]]]
After Insort 8: [[[[2]3]4[5[[6]7]]]8]
After Insort 9: [[[[2]3]4[5[[6]7]]]8[9]]
After Insort 4: [[[[2]3[4]]4[5[[6]7]]]8[9]]
After Insort 9: [[[[2]3[4]]4[5[[6]7]]]8[9[9]]]
After Insort 16: [[[[2]3[4]]4[5[[6]7]]]8[9[[9]16]]]
After Insort 25: [[[[2]3[4]]4[5[[6]7]]]8[9[[[9]16]25]]]
After Insort 36: [[[[2]3[4]]4[5[[6]7]]]8[9[[[[9]16]25]36]]]
After Insort 49: [[[[2]3[4]]4[5[[6]7]]]8[[9[[[[9]16]25]36]]49]]
After Insort 64: [[[[2]3[4]]4[5[[6]7]]]8[[9[[[[9]16]25]36]]49[64]]]
After Insort 81: [[[[2]3[4]]4[5[[6]7]]]8[[9[[[[9]16]25]36]]49[[64]81]]]
After Insort 8: [[[[2]3[4]]4[5[[6]7]]]8[8[[9[[[[9]16]25]36]]49[[64]81]]]]
After Insort 27: [[[[2]3[4]]4[5[[6]7]]]8[8[[9[[[[9]16]25[27]]36]]49[[64]81]]]]
After Insort 64: [[[[2]3[4]]4[5[[6]7]]]8[8[[[9[[[[9]16]25[27]]36]]49]64[[64]81]]]]
After Insort 16: [[[[2]3[4]]4[5[[6]7]]]8[[8[9[9]]]16[[[[[16]25[27]]36]49]64[[64]81]]]]
After Insort 81: [[[[[2]3[4]]4[5[[6]7]]]8[[8[9[9]]]16[[[[[16]25[27]]36]49]64[64]]]]81[81]]
After Insort 32: [[[[[2]3[4]]4[5[[6]7]]]8[[8[9[9]]]16[[[[[[16]25[27]]32]36]49]64[64]]]]81[81]]
After Insort 64: [[[[[2]3[4]]4[5[[6]7]]]8[[8[9[9]]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 2: [[[[[2]3[4]]4[5[[6]7]]]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 3: [[[[3[4]]4[5[[6]7]]]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 4: [[[[4]4[5[[6]7]]]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 4: [[[4[5[[6]7]]]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 5: [[[5[[6]7]]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 6: [[[[6]7]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 7: [[[7]8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 8: [[8[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]]81[81]]
Before Discard 8: [[[8[9]]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]81[81]]
Before Discard 9: [[[9]16[[[[[[16]25[27]]32]36]49]64[64[64]]]]81[81]]
Before Discard 16: [[16[[[[[[16]25[27]]32]36]49]64[64[64]]]]81[81]]
Before Discard 16: [[[[[[[16]25[27]]32]36]49]64[64[64]]]81[81]]
Before Discard 25: [[[[[[25[27]]32]36]49]64[64[64]]]81[81]]
Before Discard 27: [[[[[[27]32]36]49]64[64[64]]]81[81]]
Before Discard 32: [[[[[32]36]49]64[64[64]]]81[81]]
Before Discard 36: [[[[36]49]64[64[64]]]81[81]]
Before Discard 49: [[[49]64[64[64]]]81[81]]
Before Discard 64: [[64[64[64]]]81[81]]
Before Discard 64: [[64[64]]81[81]]
Before Discard 64: [[64]81[81]]
Before Discard 81: [81[81]]
Before Discard 81: [81]
*)
