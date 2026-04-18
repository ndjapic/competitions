# Задатак: C_Rotate_and_Sum_Query.pas

```pascal
program C_Rotate_and_Sum_Query;
const
	nn = 200 * 1000;
var
	n, q, i, i0, c, l, r: int32;
	ans: int64;
	query_type: int8;
	a: array [1 .. nn] of int32;
	s: array [0 .. nn] of int64;

begin
	readln(n, q);

	s[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		s[i] := s[i-1] + a[i];
	end;
	readln;

	i0 := 0;
	for i := 1 to q do begin
		read(query_type);
		case query_type of

			1: begin
				readln(c);
				inc(i0, c);
				if i0 >= n then dec(i0, n);
			end;

			2: begin
				readln(l, r);
				inc(l, i0);
				inc(r, i0);
				ans := 0;

				if r <= n then
					inc(ans, s[r])
				else
					inc(ans, s[r-n] + s[n]);

				if l-1 <= n then
					dec(ans, s[l-1])
				else
					dec(ans, s[l-1-n] + s[n]);

				writeln(ans);
			end;

		end;
	end;
end.

```
