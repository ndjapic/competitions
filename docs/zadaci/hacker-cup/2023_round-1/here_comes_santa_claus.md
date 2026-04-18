# Задатак: here_comes_santa_claus.pas

```pascal
program here_comes_santa_claus;
uses
	math;
const
	maxn = 100 * 1000;
var
    ntc, tci: int8;
    n, i: int32;
    ans: double;
    x, merge: array [1 .. maxn] of int64;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (x[il] <= x[ir]) then begin
                merge[i] := x[il];
                inc(il);
            end else begin
                merge[i] := x[ir];
                inc(ir);
            end;

        for i := l to r do x[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

		for i := 1 to n do read(x[i]); readln; msort(1, n);

		if n = 5 then begin
			ans := max(
				(x[5] + x[3]) / 2 - (x[1] + x[2]) / 2,
				(x[5] + x[4]) / 2 - (x[1] + x[3]) / 2
			);
		end else begin
			ans := (x[n] + x[n-1]) div 2 - (x[1] + x[2]) div 2;
		end;

		writeln('Case #', tci, ': ', ans:9:7);

    end;
end.

```
