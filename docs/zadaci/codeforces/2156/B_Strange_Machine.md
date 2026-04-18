# Задатак: B_Strange_Machine.pas

```pascal
program B_Strange_Machine;
{$MODE DELPHI}
var
	notc, tci, n, q, i, j, t, a: int32;
	s: string;
	A_only: boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);
		readln(s);

		i := 1;
		while (i <= n) and (s[i] = 'A') do inc(i);
		A_only := i > n;

		for j := 1 to q do begin
			read(a);
			if A_only then
				writeln(a)
			else begin

				i := 1;
				t := 0;

				while a > 0 do begin
					case s[i] of
						'A': dec(a);
						'B': a := a div 2;
					end;
					inc(t);
					inc(i);
					if i > n then dec(i, n);
				end;

				writeln(t);

			end;
		end;
		readln;

	end;
end.

```
