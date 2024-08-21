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
        function Merge(L, R: TSortedTreap<_T>): TSortedTreap<_T>;
        function Rank(K: _T): Integer;
        procedure SplitByRank(i: Integer; var L, R: TSortedTreap<_T>); (* Needs implementation *)
        function Insort(K: _T): TSortedTreap<_T>;
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
    FreeAndNil(Self);
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

function TSortedTreap<_T>.Merge(L, R: TSortedTreap<_T>): TSortedTreap<_T>;
begin
    if L = nil then
        Result := R
    else if R = nil then
        Result := L
    else begin
        if L.Priority < R.Priority then begin
            Result := R;
            Result.Left := L.Merge(L, Result.Left);
        end else begin
            Result := L;
            Result.Right := R.Merge(Result.Right, R);
        end;
        Result.Update;
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
        Self := Merge(L, R);
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

function TSortedTreap<_T>.Insort(K: _T): TSortedTreap<_T>;
var
    L, R: TSortedTreap<_T>;
begin
    Write('Before Insort ', K, ': '); dfs; WriteLn;
    Split(K, L, R);
    Write('After Split: '); dfs; WriteLn;
    Self := TSortedTreap<_T>.Create(K);
    Self := Merge(L, Self);
    Write('Between Merge: '); dfs; WriteLn;
    Self := Merge(Self, R);
    Write('After Insort ', K, ': '); dfs; WriteLn;
    Result := Self;
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
            R := R.Merge(M, R);
    end;

    Self := Merge(L, R);
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
Standard Input
23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	0
Exec Time	0 ms
Memory	1508 KB

Standard Output
Before Insort 3: [2]
After Split: [2]
Between Merge: [2[3]]
After Insort 3: [2[3]]
Before Insort 4: [2[3]]
After Split: [2[3]]
Between Merge: [2[3[4]]]
After Insort 4: [2[3[4]]]
Before Insort 5: [2[3[4]]]
After Split: [2[3[4]]]
Between Merge: [2[[3[4]]5]]
After Insort 5: [2[[3[4]]5]]
Before Insort 6: [2[[3[4]]5]]
After Split: [2[[3[4]]5]]
Between Merge: [2[[[3[4]]5]6]]
After Insort 6: [2[[[3[4]]5]6]]
Before Insort 7: [2[[[3[4]]5]6]]
After Split: [2[[[3[4]]5]6]]
Between Merge: [2[[[[3[4]]5]6]7]]
After Insort 7: [2[[[[3[4]]5]6]7]]
Before Insort 8: [2[[[[3[4]]5]6]7]]
After Split: [2[[[[3[4]]5]6]7]]
Between Merge: [[2[[[[3[4]]5]6]7]]8]
After Insort 8: [[2[[[[3[4]]5]6]7]]8]
Before Insort 9: [[2[[[[3[4]]5]6]7]]8]
After Split: [[2[[[[3[4]]5]6]7]]8]
Between Merge: [[2[[[[3[4]]5]6]7]]8[9]]
After Insort 9: [[2[[[[3[4]]5]6]7]]8[9]]
Before Insort 4: [[2[[[[3[4]]5]6]7]]8[9]]
After Split: [[[[[4]5]6]7]8[9]]
Between Merge: [2[[3]4]]
After Insort 4: [[2[[[[[3]4[4]]5]6]7]]8[9]]
Before Insort 9: [[2[[[[[3]4[4]]5]6]7]]8[9]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[9]]
After Insort 9: [[2[[[[[3]4[4]]5]6]7]]8[9[9]]]
Before Insort 16: [[2[[[[[3]4[4]]5]6]7]]8[9[9]]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[9[9]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16]]]
After Insort 16: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16]]]
Before Insort 25: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16]]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25]]]]
After Insort 25: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25]]]]
Before Insort 36: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25]]]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[36]]]]]
After Insort 36: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[36]]]]]
Before Insort 49: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[36]]]]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[36]]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[[36]49]]]]]
After Insort 49: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[[36]49]]]]]
Before Insort 64: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[[36]49]]]]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[9[[9]16[25[[36]49]]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64]]
After Insort 64: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64]]
Before Insort 81: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64]]
After Split: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64]]
Between Merge: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64[81]]]
After Insort 81: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64[81]]]
Before Insort 8: [[2[[[[[3]4[4]]5]6]7]]8[[9[[9]16[25[[36]49]]]]64[81]]]
After Split: [8[[9[[9]16[25[[36]49]]]]64[81]]]
Between Merge: [2[[[[[3]4[4]]5]6]7[8]]]
After Insort 8: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25[[36]49]]]]64[81]]]
Before Insort 27: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25[[36]49]]]]64[81]]]
After Split: [[2[[[[[3]4[4]]5]6]7[8]]]8[9[[9]16[25]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25]]]27]]
After Insort 27: [[2[[[[[3]4[4]]5]6]7[8]]]8[[[9[[9]16[25]]]27[[36]49]]64[81]]]
Before Insort 64: [[2[[[[[3]4[4]]5]6]7[8]]]8[[[9[[9]16[25]]]27[[36]49]]64[81]]]
After Split: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25]]]27[[36]49]]]
Between Merge: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25]]]27[[36]49]]]64]
After Insort 64: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25]]]27[[36]49]]]64[64[81]]]
Before Insort 16: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9]16[25]]]27[[36]49]]]64[64[81]]]
After Split: [[[16[25]]27[[36]49]]64[64[81]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7[8]]]8[9[9[16]]]]
After Insort 16: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64[81]]]
Before Insort 81: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64[81]]]
After Split: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64]]
Between Merge: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64[81]]]
After Insort 81: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64[81[81]]]]
Before Insort 32: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[[36]49]]]64[64[81[81]]]]
After Split: [[[36]49]64[64[81[81]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[32]]]
After Insort 32: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[32[[36]49]]]]64[64[81[81]]]]
Before Insort 64: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[32[[36]49]]]]64[64[81[81]]]]
After Split: [64[64[81[81]]]]
Between Merge: [[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[32[[[36]49]64]]]]
After Insort 64: [[[2[[[[[3]4[4]]5]6]7[8]]]8[[9[[9[16]]16[25]]]27[32[[[36]49]64]]]]64[64[81[81]]]]
2 3 4 4 5 6 7 8 8 9 9 16 16 25 27 32 36 49 64 64 64 81 81
*)
