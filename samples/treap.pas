program SortedTreap;
{$mode delphi}{$inline on}

type
    TSortedTreap<_T> = class
        Left, Right: TSortedTreap<_T>;
        Key: _T;
        Priority: Integer;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Integer; inline; static;
        class function GetSize(Other: TSortedTreap<_T>): Integer; inline; static;
        procedure Update; inline;
        constructor Create(K: _T);
        destructor Destroy; override;
        procedure Split(K: _T; var L, R: TSortedTreap<_T>);
        constructor Merge(L, R: TSortedTreap<_T>);
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap<_T>);
        constructor Insort(K: _T);
        constructor Discard(K: _T);
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
        {Self := TSortedTreap<_T>.Merge(L, R);}
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
    Split(K, L, R);
    Self := TSortedTreap<_T>.Create(K);
    Self := TSortedTreap<_T>.Merge(L, Self);
    Self := TSortedTreap<_T>.Merge(Self, R);
    Write('After Insort ', K, ': '); dfs; WriteLn;
end;

constructor TSortedTreap<_T>.Discard(K: _T);
var
    L, M, R: TSortedTreap<_T>;
begin
    Split(K, L, R);

    if R <> nil then begin
        R.SplitByRank(1, M, R);
        if Compare(M.Key, K) = 0 then begin
            M.Free;
            M := nil;
        end else if R = nil then
            R := M
        else
            R := TSortedTreap<_T>.Merge(M, R);
    end;

    Self := TSortedTreap<_T>.Merge(L, R);
    Write('After Discard ', K, ': '); dfs; WriteLn;
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

    a := a.Discard(9); // Delete one of two nines.
    a := a.Discard(10); // Does nothing.
    n := a.Size; // Update n.

    for i := 0 to n-2 do begin
      a := a.Discard(a[0]);
      {Write('After Discard ', a[0], ': '); a.dfs; WriteLn;}
    end;
    a.Free;
end.

(*
Standard Input

23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	0
Exec Time	0 ms
Memory	1628 KB

Standard Output

After Insort 3: [[2]3]
After Insort 4: [[[2]3]4]
After Insort 5: [[[2]3]4[5]]
After Insort 6: [[[2]3]4[5[6]]]
After Insort 7: [[[2]3]4[5[[6]7]]]
After Insort 8: [[[2]3]4[5[[6]7[8]]]]
After Insort 9: [[[2]3]4[5[[6]7[[8]9]]]]
After Insort 4: [[[2]3[4]]4[5[[6]7[[8]9]]]]
After Insort 9: [[[[2]3[4]]4[5[[6]7[8]]]]9[9]]
After Insort 16: [[[[2]3[4]]4[5[[6]7[8]]]]9[[9]16]]
After Insort 25: [[[[2]3[4]]4[5[[6]7[8]]]]9[[9]16[25]]]
After Insort 36: [[[[2]3[4]]4[5[[6]7[8]]]]9[[[9]16[25]]36]]
After Insort 49: [[[[2]3[4]]4[5[[6]7[8]]]]9[[[9]16[25]]36[49]]]
After Insort 64: [[[[2]3[4]]4[5[[6]7[8]]]]9[[[9]16[25]]36[49[64]]]]
After Insort 81: [[[[2]3[4]]4[5[[6]7[8]]]]9[[[9]16[25]]36[49[[64]81]]]]
After Insort 8: [[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9]16[25]]36[49[[64]81]]]]
After Insort 27: [[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9]16[25]]27[36[49[[64]81]]]]]
After Insort 64: [[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9]16[25]]27[36[49[[[64]64]81]]]]]
After Insort 16: [[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9[16]]16[25]]27[36[49[[[64]64]81]]]]]
After Insort 81: [[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9[16]]16[25]]27[36[49[[[64]64]81[81]]]]]]
After Insort 32: [[[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9[16]]16[25]]27]]32[36[49[[[64]64]81[81]]]]]
After Insort 64: [[[[[2]3[4]]4[5[[6]7[[8]8]]]]9[[[9[16]]16[25]]27]]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 9: [[[[[2]3[4]]4[5[[[6]7[[[8]8]9[16]]]16[25]]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 10: [[[[[2]3[4]]4[5[[[6]7[[[8]8]9[16]]]16[25]]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 2: [[[[3[4]]4[5[[[6]7[[[8]8]9[16]]]16[25]]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 3: [[[[4]4[5[[[6]7[[[8]8]9[16]]]16[25]]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 4: [[[4[5[[[6]7[[[8]8]9[16]]]16[25]]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 4: [[[5[[[6]7[[[8]8]9[16]]]16[25]]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 5: [[[[[6]7[[[8]8]9[16]]]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 6: [[[[7[[[8]8]9[16]]]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 7: [[[[[[8]8]9[16]]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 8: [[[[[8]9[16]]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 8: [[[[9[16]]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 9: [[[[16]16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 16: [[[16[25]]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 16: [[[25]27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 25: [[27]32[[36[49]]64[[[64]64]81[81]]]]
After Discard 27: [32[[36[49]]64[[[64]64]81[81]]]]
After Discard 32: [[36[49]]64[[[64]64]81[81]]]
After Discard 36: [[49]64[[[64]64]81[81]]]
After Discard 49: [64[[[64]64]81[81]]]
After Discard 64: [[[64]64]81[81]]
After Discard 64: [[64]81[81]]
After Discard 64: [81[81]]
After Discard 81: [81]
*)
