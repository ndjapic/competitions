# Задатак: C_Those_Who_Are_With_Us.pas

```pascal
program C_Those_Who_Are_With_Us;
{$INLINE ON}
uses
	math;
const
	nn = 100 * 1000;
var
    ntc, tci, n, m, i, j: int32;
    x, ans: int8;
    a, r, c, ll, lr, rl, rr: array [1 .. nn] of int8;

function v(i, j: int32): int32; inline;
begin
	v := (i-1)*m + j;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

		for j := 1 to m do begin
			c[j] := 1;
			ll[v(1, j)] := 0;
			lr[v(1, j)] := 0;
			rl[v(n, j)] := 0;
			rr[v(n, j)] := 0;
		end;

		for i := 1 to n do begin
			r[i] := 1;
			for j := 1 to m do begin
				read(x);
				r[i] := max(r[i], x);
				c[j] := max(c[j], x);
				a[v(i, j)] := x;
			end;
			readln;
		end;

		for i := 2 to n do begin

			ll[v(i, 1)] := 0;
			lr[v(i, m)] := 0;

			for j := 2 to m do begin
				x := max(ll[v(i, j-1)], ll[v(i-1, j)]);
				ll[v(i, j)] := max(x, a[v(i-1, j-1)]);
			end;

			for j := m-1 downto 1 do begin
				x := max(lr[v(i, j+1)], lr[v(i-1, j)]);
				lr[v(i, j)] := max(x, a[v(i-1, j+1)]);
			end;

		end;

		for i := n-1 downto 1 do begin

			rl[v(i, 1)] := 0;
			rr[v(i, m)] := 0;

			for j := 2 to m do begin
				x := max(rl[v(i, j-1)], rl[v(i+1, j)]);
				rl[v(i, j)] := max(x, a[v(i+1, j-1)]);
			end;

			for j := m-1 downto 1 do begin
				x := max(rr[v(i, j+1)], rr[v(i+1, j)]);
				rr[v(i, j)] := max(x, a[v(i+1, j+1)]);
			end;

		end;

		ans := 100;
		for i := 1 to n do
			for j := 1 to m do begin
				x := max(r[i], c[j]) - 1;
				x := max(x, ll[v(i, j)]);
				x := max(x, lr[v(i, j)]);
				x := max(x, rl[v(i, j)]);
				x := max(x, rr[v(i, j)]);
				ans := min(ans, x);
			end;

		writeln(ans);

    end;
end.

```
