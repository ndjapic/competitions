# Задатак: B_Team_Training.pas

```pascal
program B_Team_Training;
type
    tarr = array of int32;
var
    ntc, tci: int16;
    n, x, i, l, r, ans: int32;
    a, cp: tarr;

procedure msort(var arr: tarr; l, r: int32);
(* Usage:
	setlength(cp, length(arr));
	msort(arr, 0, length(arr));
*)
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (arr[il] >= arr[ir]) then begin
                cp[i] := arr[il];
                inc(il);
            end else begin
                cp[i] := arr[ir];
                inc(ir);
            end;

        for i := l to r-1 do arr[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);
        setlength(a, n);
        setlength(cp, n);

        for i := 0 to n-1 do read(a[i]); readln;
        msort(a, 0, n);

        ans := 0;
        l := -1;
        for r := 0 to n-1 do
            if int64(r-l) * a[r] >= x then begin
                inc(ans);
                l := r;
            end;

        writeln(ans);

    end;
end.

```
