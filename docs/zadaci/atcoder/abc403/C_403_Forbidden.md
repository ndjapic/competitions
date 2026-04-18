# Задатак: C_403_Forbidden.pas

```pascal
program C_403_Forbidden;
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
    n, m, q, i, k, tp, x, y, z, rank, size: int32;
    users: array [1 .. nn] of boolean;
    pages: array [1 .. nn] of TAVLTree<int32>;

begin
    readln(n, m, q);

    for i := 1 to n do begin
        users[i] := false;
        pages[i] := nil;
    end;

    for k := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                read(x, y);
                TAVLTree<int32>.Add(pages[x], y);
            end;

            2: begin
                read(x);
                users[x] := true;
            end;

            3: begin
                read(x, y);
                if users[x] then
                    writeln('Yes')
                else begin
                    rank := TAVLTree<int32>.GetRank(pages[x], y);
                    size := 0;
                    if pages[x] <> nil then
                        size := TAVLTree<int32>.GetTreeSize(pages[x]);
                    if rank < size then begin
                        TAVLTree<int32>.GetAt(pages[x], rank, z);
                        if y = z then
                            writeln('Yes')
                        else
                            writeln('No');
                    end else
                        writeln('No');
                end;

            end;

        end;
        readln;
    end;
end.

```
