program F1_Dyn_scripted_Robot;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;
const
    nn = 1000 * 1000;

type
    generic tcomparer<_t> = class
    public
        function LessOrEqual(constref lhs, rhs: _t): boolean; inline;
    end;
    generic tlist<_t, _c> = class
    private
        fitems, items2: array of _t;
        count: sizeint;
        function getItem(index: SizeInt): _t;
        procedure setItem(index: SizeInt; item: _t);
    public
        constructor create();
        destructor destroy(); override; // allows the use of a parent class destroyer
        function isNonIncreasing(lend, rend: sizeint; cmp: _c): boolean; inline;
        procedure MergeSort(lend, rend: sizeint; cmp: _c; stable: boolean);
        procedure sort(cmp: _c; stable: boolean);
        function bisectr(x: _t; cmp: _c): sizeint;
        property items[index: sizeint]: _t read getItem write setItem;
    end;

type
    icomparer = specialize tcomparer<int64>;
    ilist = specialize tlist<int64, icomparer>;

var
    ntc, tci: int16;
    n, k, w, h, w2, h2, x, y, dx, dy, i: int32;
    p, ans: int64;
    s: string;
    cmp: icomparer;
    points: ilist;

function tcomparer.LessOrEqual(constref lhs, rhs: _t): boolean; inline;
begin
    result := lhs <= rhs;
end;

function tlist.getItem(index: SizeInt): _t;
begin
    result := fitems[index];
end;

procedure tlist.setItem(index: SizeInt; item: _t);
begin
    if length(fitems) <= index then setlength(fitems, 2 * index);
    fitems[index] := item;
    count := max(count, index + 1);
end;

constructor tlist.create();
begin
    setlength(fitems, 1000);
    count := 0;
end;

destructor tlist.destroy();
begin
    setlength(fitems, 0);
    setlength(items2, 0);
    count := 0;
    inherited; // Also called parent class destroyer
end;

function tlist.isNonIncreasing(lend, rend: sizeint; cmp: _c): boolean; inline;
var
    j: sizeInt;
begin
    j := lend + 1;
    while (j < rend) and cmp.LessOrEqual(fitems[j], fitems[j-1]) do inc(j);
    result := j >= rend;
end;

procedure tlist.MergeSort(lend, rend: sizeint; cmp: _c; stable: boolean);
var
    i, l, r, m: sizeint;

begin
    i := lend + 1;
    while (i < rend) and cmp.LessOrEqual(fitems[i-1], fitems[i]) do inc(i);

    if i < rend then begin

        if not stable and isNonIncreasing(lend, rend, cmp) then begin

            l := lend;
            r := rend - 1;
            while l < r do begin
                items2[l] := fitems[l];
                fitems[l] := fitems[r];
                fitems[r] := items2[l];
                inc(l);
                dec(r);
            end;

        end else begin

            m := (lend + rend) div 2;
            if i < m then MergeSort(lend, m, cmp, stable);
            MergeSort(m, rend, cmp, stable);

            l := lend;
            r := m;
            for i := lend to rend - 1 do
                if (r = rend) or (l < m) and cmp.LessOrEqual(fitems[l], fitems[r]) then begin
                    items2[i] := fitems[l];
                    inc(l);
                end else begin
                    items2[i] := fitems[r];
                    inc(r);
                end;

            for i := lend to rend - 1 do fitems[i] := items2[i];
        end;
    end;
end;

procedure tlist.sort(cmp: _c; stable: boolean);
begin
    if length(items2) < length(fitems) then
        setlength(items2, length(fitems));
    MergeSort(0, count, cmp, stable);
end;

function tlist.bisectr(x: _t; cmp: _c): sizeint;
var
    l, r, m: sizeint;
begin
    l := -1;
    r := count;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if cmp.LessOrEqual(items[m], x) then
            l := m
        else
            r := m;
    end;
    bisectr := r;
end;

begin
    cmp := icomparer.create(); // Initialize the object by calling the class builder
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, w, h);
        readln(s);
        w2 := w*2;
        h2 := h*2;

        points := ilist.create();

        x := 0;
        y := 0;
        for i := 1 to n do begin

            case s[i] of

                'L': begin
                    if x = 0 then x := w2;
                    dec(x);
                end;

                'R': begin
                    inc(x);
                    if x = w2 then x := 0;
                end;

                'U': begin
                    inc(y);
                    if y = h2 then y := 0;
                end;

                'D': begin
                    if y = 0 then y := h2;
                    dec(y);
                end;

            end;

            p := (int64(x) shl 21) + y;
            points.items[points.count] := p;

        end;
        points.sort(cmp, false);

        dx := x;
        dy := y;

        x := 0;
        y := 0;
        ans := 0;
        for i := 1 to k do begin

            p := (int64(x) shl 21) + y;
            inc(ans, points.bisectr(p, cmp) - points.bisectr(p-1, cmp));
            dec(x, dx);
            dec(y, dy);
            if x < 0 then inc(x, w2);
            if y < 0 then inc(y, h2);

        end;

        writeln(ans);

        points.free(); // Free invites your own class Destroy discharger

    end;
    cmp.free();
end.
