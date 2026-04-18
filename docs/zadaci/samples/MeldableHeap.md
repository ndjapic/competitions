# Задатак: MeldableHeap.pas

```pascal
program MeldableHeap;
{$mode delphi}{$inline on}

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
    Result := lhs - rhs;
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
    n, i, K: Integer;
    pq: TIntHeap;

begin
    Randomize;
    pq := nil;
    ReadLn(n);

    for i := 0 to n-1 do begin
        Read(K);
        TIntHeap.Push(pq, K);
    end;
    ReadLn;

    while TIntHeap.GetSize(pq) > 0 do begin
        TIntHeap.Pop(pq, K);
        Write(K:3);
    end;
    WriteLn;

    pq.Free;
end.

(*
Standard Input

23
2 3 4 5 6 7 8 9 4 9 16 25 36 49 64 81 8 27 64 16 81 32 64

Exit Code	0
Exec Time	0 ms
Memory	1504 KB

Standard Output

  2  3  4  4  5  6  7  8  8  9  9 16 16 25 27 32 36 49 64 64 64 81 81
*)

```
