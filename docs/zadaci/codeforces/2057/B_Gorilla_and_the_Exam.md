# Задатак: B_Gorilla_and_the_Exam.pas

```pascal
program B_Gorilla_and_the_Exam;
{$mode delphi}{$inline on}
uses
    math;
const
    nn = 100 * 1000;

type
    TMeldableHeap<_T> = class
        Left, Right: TMeldableHeap<_T>;
        Key: _T;
        Size: Integer;
        class function Compare(lhs, rhs: _T): Int32; inline; static;
        class function GetSize(Other: TMeldableHeap<_T>): Integer; inline; static;
        constructor Create(K: _T);
        destructor Destroy; override;
        class function Merge(var L, R: TMeldableHeap<_T>): TMeldableHeap<_T>; static;
        class procedure Push(var L: TMeldableHeap<_T>; K: _T); static;
        class procedure Pop(var M: TMeldableHeap<_T>; var K: _T); static;
    end;
    TIntHeap = TMeldableHeap<Int32>;

(* BEGIN TMeldableHeap *)

class function TMeldableHeap<_T>.Compare(lhs, rhs: _T): Int32;
begin
    Result := - lhs + rhs;
end;

class function TMeldableHeap<_T>.GetSize(Other: TMeldableHeap<_T>): Integer;
begin
    if Other = nil then
        Result := 0
    else
        Result := Other.Size;
end;

constructor TMeldableHeap<_T>.Create(K: _T);
begin
    Inherited Create;
    Key := K;
    Size := 1;
    Left := nil;
    Right := nil;
end;

destructor TMeldableHeap<_T>.Destroy;
begin
    if Left <> nil then Left.Free;
    if Right <> nil then Right.Free;
    Inherited;
    Self := nil;
end;

class function TMeldableHeap<_T>.Merge(var L, R: TMeldableHeap<_T>): TMeldableHeap<_T>;
var
    M: TMeldableHeap<_T>;
begin
    if L = nil then
        Result := R
    else if R = nil then
        Result := L
    else begin
        if TMeldableHeap<_T>.Compare(R.Key, L.Key) < 0 then begin
            M := L;
            L := R;
            R := M;
        end;
        if odd(Random(2)) then
            L.Left := TMeldableHeap<_T>.Merge(L.Left, R)
        else
            L.Right := TMeldableHeap<_T>.Merge(L.Right, R);
        Result := L;
    end;
    if Result <> nil then
        Result.Size := 1 + GetSize(Result.Left) + GetSize(Result.Right);
end;

class procedure TMeldableHeap<_T>.Push(var L: TMeldableHeap<_T>; K: _T);
var
    R: TMeldableHeap<_T>;
begin
    R := TMeldableHeap<_T>.Create(K);
    L := Merge(L, R);
end;

class procedure TMeldableHeap<_T>.Pop(var M: TMeldableHeap<_T>; var K: _T);
begin
    K := M.Key;
    M := Merge(M.Left, M.Right);
end;

(* END TMeldableHeap *)

var
    ntc, tci: int16;
    n, k, m, i: int32;
    a, c: array [1 .. nn] of int32;
    pq: TIntHeap;

begin
    Randomize;
    pq := nil;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do begin
            read(a[i]);
            TIntHeap.Push(pq, a[i]);
        end;
        readln;

        m := 1;
        c[1] := 1;
        TIntHeap.Pop(pq, a[1]);

        for i := 2 to n do begin
            TIntHeap.Pop(pq, a[i]);
            if a[i-1] <> a[i] then begin
                inc(m);
                c[m] := 0;
            end;
            inc(c[m]);
        end;

        for i := 1 to m do TIntHeap.Push(pq, c[i]);
        for i := 1 to m do TIntHeap.Pop(pq, c[i]);

        while (k >= c[m]) and (m > 1) do begin
            dec(k, c[m]);
            dec(m);
        end;

        writeln(m);

    end;
end.

```
