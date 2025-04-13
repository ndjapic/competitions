program AVLTree;
{$MODE DELPHI}{$INLINE ON}
uses
    math;
const
    nn = 1000 * 1000;

type
    TAVLTree<_T> = class
        x: _T;
        c: int32;
        h: int8;
        l, r: TAVLTree<_T>;
        constructor Create(x: _T);
        destructor Destroy; override;
        class function GetCount(t: TAVLTree<_T>): int32; inline;
        class function GetHeight(t: TAVLTree<_T>): int8;
        function GetBalance(): int8; inline;
        procedure UpdateNode(); inline;
        class procedure RotateL(var t: TAVLTree<_T>); inline;
        class procedure RotateR(var t: TAVLTree<_T>); inline;
        class procedure Add(var t: TAVLTree<_T>; x: _T);
        {class procedure Discard(var t: TAVLTree<_T>; x: _T);
        class procedure GetAt(t: TAVLTree<_T>; i: int32; var x: _T);
        class function GetRank(t: TAVLTree<_T>; x: _T): int32;}
    end;

constructor TAVLTree<_T>.Create(x: _T);
begin
    Self.x := x;
    c := 1;
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

class function TAVLTree<_T>.GetCount(t: TAVLTree<_T>): int32;
begin
    if t = nil then
        Result := 0
    else
        Result := t.c;
end;

class function TAVLTree<_T>.GetHeight(t: TAVLTree<_T>): int8;
begin
    if t = nil then
        Result := 0
    else
        Result := t.c;
end;

function TAVLTree<_T>.GetBalance(): int8;
begin
    if Self = nil then
        Result := 0
    else
        Result := GetHeight(r) - GetHeight(l);
end;

procedure TAVLTree<_T>.UpdateNode();
begin
    c := GetCount(l) + GetCount(r) + 1;
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
    r.UpdateNode();
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
    l.UpdateNode();
    t := l;
end;

class procedure TAVLTree<_T>.Add(var t: TAVLTree<_T>; x: _T);
begin
    if t = nil then
        t := Create(x)
    else if x < t.x then begin
        Add(t.l, x);
        if t.GetBalance() < -1 then begin
            if t.l.GetBalance() > 0 then RotateL(t.l);
            RotateR(t);
        end;
    end else begin
        Add(t.r, x);
        if t.GetBalance() > -1 then begin
            if t.r.GetBalance() < 0 then RotateR(t.r);
            RotateL(t);
        end;
    end;
end;

var
    i: int32;
    t: TAVLTree<int32>;

begin
    randomize;
    t := nil;

    for i := 1 to nn do TAVLTree<int32>.Add(t, random(nn));
    writeln(
        ' c=', TAVLTree<int32>.GetCount(t),
        ' h=', TAVLTree<int32>.GetHeight(t)
    );

    if t <> nil then t.Free;

    {try
        readln(n);
        for i := 0 to n-1 do begin
            read(x);
            TAVLTree<int32>.Add(t, x);
        end;
        readln;

        for i := 0 to TAVLTree<int32>.GetCount(t) - 1 do begin
            TAVLTree<int32>.GetAt(t, i, x);
            write(' ', x);
        end;
        writeln;
    finally
        t.Free;
    end;}
end.

(*
Улазни подаци:
14
2 4 8 16 32 64 3 9 27 81 5 25 7 49
*)
