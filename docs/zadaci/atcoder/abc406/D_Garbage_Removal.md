# Задатак: D_Garbage_Removal.pas

```pascal
program D_Garbage_Removal;
{$MODE DELPHI}{$INLINE ON}
uses
    math;
const
    nn = 200 * 1000;

type
    TAVLTree<_T> = class
        x: _T;
        elementCount, treeSize: int32;
        h: int8;
        l, r: TAVLTree<_T>;
        class function Compare(lhs, rhs: _T): SizeInt; inline;
        constructor Create(x: _T);
        destructor Destroy; override;
        class function GetTreeSize(t: TAVLTree<_T>): int32; inline;
        class function GetHeight(t: TAVLTree<_T>): int8;
        function GetBalance(): int8; inline;
        class function GetElementCount(t: TAVLTree<_T>; x: _T): int32;
        procedure UpdateNode(); inline;
        class procedure RotateL(var t: TAVLTree<_T>); inline;
        class procedure RotateR(var t: TAVLTree<_T>); inline;
        class procedure Add(var t: TAVLTree<_T>; x: _T);
        class procedure Discard(var t: TAVLTree<_T>; x: _T);
        class procedure GetAt(t: TAVLTree<_T>; i: int32; var x: _T);
        class function GetRank(t: TAVLTree<_T>; x: _T): int32;
    end;

class function TAVLTree<_T>.Compare(lhs, rhs: _T): SizeInt;
begin
    Result := lhs - rhs;
end;

constructor TAVLTree<_T>.Create(x: _T);
begin
    Self.x := x;
    elementCount := 1;
    treeSize := 1;
    h := 1;
    l := nil;
    r := nil;
end;

destructor TAVLTree<_T>.Destroy;
begin
    if l <> nil then l.Free;
    if r <> nil then r.Free;
    inherited;
end;

class function TAVLTree<_T>.GetTreeSize(t: TAVLTree<_T>): int32;
begin
    if t = nil then
        Result := 0
    else
        Result := t.treeSize;
end;

class function TAVLTree<_T>.GetHeight(t: TAVLTree<_T>): int8;
begin
    if t = nil then
        Result := 0
    else
        Result := t.h;
end;

function TAVLTree<_T>.GetBalance(): int8;
begin
    {if Self = nil then
        Result := 0
    else}
        Result := GetHeight(r) - GetHeight(l);
end;

class function TAVLTree<_T>.GetElementCount(t: TAVLTree<_T>; x: _T): int32;
begin
    if t = nil then
        Result := 0
    else if Compare(x, t.x) < 0 then
        Result := GetElementCount(t.l, x)
    else if Compare(x, t.x) > 0 then
        Result := GetElementCount(t.r, x)
    else
        Result := t.elementCount;
end;

procedure TAVLTree<_T>.UpdateNode();
begin
    treeSize := GetTreeSize(l) + GetTreeSize(r) + elementCount;
    h := max(GetHeight(l), GetHeight(r)) + 1;
end;

class procedure TAVLTree<_T>.RotateL(var t: TAVLTree<_T>);
var
    r: TAVLTree<_T>;
begin
    r := t.r;
    t.r := r.l;
    r.l := t;
    t.UpdateNode();
    {r.UpdateNode(); Is this neccessary? There will be update after this.}
    t := r;
end;

class procedure TAVLTree<_T>.RotateR(var t: TAVLTree<_T>);
var
    l: TAVLTree<_T>;
begin
    l := t.l;
    t.l := l.r;
    l.r := t;
    t.UpdateNode();
    {l.UpdateNode(); Is this neccessary? There will be update after this.}
    t := l;
end;

class procedure TAVLTree<_T>.Add(var t: TAVLTree<_T>; x: _T);
begin
    if t = nil then
        t := TAVLTree<int32>.Create(x)
    else if Compare(x, t.x) < 0 then begin

        Add(t.l, x);
        if t.GetBalance() < -1 then begin
            if t.l.GetBalance() > 0 then RotateL(t.l);
            RotateR(t);
        end;

    end else if Compare(x, t.x) > 0 then begin

        Add(t.r, x);
        if t.GetBalance() > 1 then begin
            if t.r.GetBalance() < 0 then RotateR(t.r);
            RotateL(t);
        end;

    end else
        inc(t.elementCount);

    t.UpdateNode();
end;

class procedure TAVLTree<_T>.Discard(var t: TAVLTree<_T>; x: _T);
var
    temp: TAVLTree<_T>;
begin
    if t <> nil then begin
        if Compare(x, t.x) < 0 then
            Discard(t.l, x)
        else if Compare(x, t.x) > 0 then
            Discard(t.r, x)
        else if t.elementCount > 1 then
            dec(t.elementCount)
        else if t.l = nil then begin
            temp := t;
            t := t.r;
            temp.r := nil; // Prevent recursive Destroy
            temp.Free;
        end else if t.r = nil then begin
            temp := t;
            t := t.l;
            temp.l := nil; // Prevent recursive Destroy
            temp.Free;
        end else begin // Чвор са два потомка
            if GetHeight(t.l) > GetHeight(t.r) then begin
                RotateR(t);
                Discard(t.r, x);
            end else begin
                RotateL(t);
                Discard(t.l, x);
            end;
            Exit; // Након ротације и рекурзивног брисања, балансирање ће се обавити при повратку
        end;

        if t <> nil then begin
            t.UpdateNode();
            if t.GetBalance() < -1 then begin
                if (t.l <> nil) and (t.l.GetBalance() > 0) then RotateL(t.l);
                RotateR(t);
                t.UpdateNode(); // Re-update after rotation
            end else if t.GetBalance() > 1 then begin
                if (t.r <> nil) and (t.r.GetBalance() < 0) then RotateR(t.r);
                RotateL(t);
                t.UpdateNode(); // Re-update after rotation
            end;
        end;
    end;
end;

class procedure TAVLTree<_T>.GetAt(t: TAVLTree<_T>; i: int32; var x: _T);
var
    leftSize: int32;
begin
    if t <> nil then begin
        leftSize := GetTreeSize(t.l);
        if i < leftSize then
            GetAt(t.l, i, x)
        else if i < leftSize + t.elementCount then
            x := t.x
        else
            GetAt(t.r, i - leftSize - t.elementCount, x);
    end;
end;

class function TAVLTree<_T>.GetRank(t: TAVLTree<_T>; x: _T): int32;
begin
    Result := 0;
    while t <> nil do begin
        if Compare(x, t.x) < 0 then
            t := t.l
        else if Compare(x, t.x) > 0 then begin
            Result := Result + GetTreeSize(t.l) + t.elementCount;
            t := t.r;
        end else begin
            Result := Result + GetTreeSize(t.l);
            Exit;
        end;
    end;
end;

var
    h, w, n, q, i, k, x, y, ans: int32;
    tp: int8;
    row, col: array [1 .. nn] of TAVLTree<int32>;

begin
    readln(h, w, n);

    for x := 1 to h do row[x] := nil;
    for y := 1 to w do col[y] := nil;

    for i := 1 to n do begin
        readln(x, y);
        TAVLTree<int32>.Add(row[x], y);
        TAVLTree<int32>.Add(col[y], x);
    end;

    readln(q);
    for k := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                readln(x);
                ans := TAVLTree<int32>.GetTreeSize(row[x]);

                for i := 0 to ans-1 do begin
                    TAVLTree<int32>.GetAt(row[x], 0, y);
                    TAVLTree<int32>.Discard(row[x], y);
                    TAVLTree<int32>.Discard(col[y], x);
                end;
            end;

            2: begin
                readln(y);
                ans := TAVLTree<int32>.GetTreeSize(col[y]);

                for i := 0 to ans-1 do begin
                    TAVLTree<int32>.GetAt(col[y], 0, x);
                    TAVLTree<int32>.Discard(col[y], x);
                    TAVLTree<int32>.Discard(row[x], y);
                end;
            end;

        end;
        writeln(ans);
    end;

    for x := 1 to h do
        if row[x] <> nil then row[x].Free;

    for y := 1 to w do
        if col[y] <> nil then col[y].Free;
end.

```
