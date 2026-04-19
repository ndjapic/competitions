# Problem: C_Median_Splits.pas

```pascal
program C_Median_Splits;
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
    ntc, tci: int16;
    n, i, k, m, x: int32;
    a, lm, rm: array [1 .. nn] of int32;
    s: TAVLTree<int32>;

begin
    randomize;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do read(a[i]); readln;

        s := nil;
        for i := 1 to n do begin
            TAVLTree<int32>.Add(s, a[i]);
            TAVLTree<int32>.GetAt(s, (i-1) div 2, x);
            lm[i] := x;
        end;
        s.Free;

        s := nil;
        for i := n downto 1 do begin
            TAVLTree<int32>.Add(s, a[i]);
            TAVLTree<int32>.GetAt(s, (i-1) div 2, x);
            rm[n+1-i] := x;
        end;
        s.Free;

        m := k+1;

        for i := 2 to n-1 do begin
            s := nil;
            TAVLTree<int32>.Add(s, a[i]);
            TAVLTree<int32>.Add(s, lm[i-1]);
            TAVLTree<int32>.Add(s, rm[n-i]);
            TAVLTree<int32>.GetAt(s, 1, x);
            m := min(m, x);
            s.Free;
        end;

        for i := 2 to n-2 do begin
            s := nil;
            TAVLTree<int32>.Add(s, min(a[i], a[i+1]));
            TAVLTree<int32>.Add(s, lm[i-1]);
            TAVLTree<int32>.Add(s, rm[n-i-1]);
            TAVLTree<int32>.GetAt(s, 1, x);
            m := min(m, x);
            s.Free;
        end;

        for i := 2 to n do begin
            lm[i] := min(lm[i], lm[i-1]);
            rm[i] := min(rm[i], rm[i-1]);
        end;

        for i := 2 to n-1 do
            m := min(m, max(lm[i-1], rm[n-i]));

        if m <= k then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
