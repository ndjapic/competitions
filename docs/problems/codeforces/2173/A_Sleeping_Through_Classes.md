# Problem: A_Sleeping_Through_Classes.pas

```pascal
program A_Sleeping_Through_Classes;
const
	nn = 100;
var
	notc, tci, n, k, l, r, ans: int32;
	wake: array [0 .. nn] of int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s);

		for l := 0 to n do wake[l] := 0;

		ans := 0;
		for l := 1 to n do begin
			if s[l] = '1' then begin
				inc(wake[l]);
				r := l+k+1;
				if r <= n then
					dec(wake[r]);
			end;
			inc(wake[l], wake[l-1]);
			if wake[l] = 0 then inc(ans);
		end;
		writeln(ans);

	end;
end.

```
