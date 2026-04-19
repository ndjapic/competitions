# Problem: B_Deque_Process.pas

```pascal
program B_Deque_Process;
{$MODE DELPHI}{$INLINE ON}
const
	nn = 100 * 1000;
var
	ntc, tci, n, i, l, r: int32;
	increasing: boolean;
	s: string;
	p: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		setlength(s, n);

		for i := 1 to n do read(p[i]);

		l := 1;
		r := n;
		i := 0;
		increasing := true;
		while l < r do begin
			if (p[l] < p[r]) = increasing then begin

				inc(i);
				s[i] := 'L';
				inc(i);
				s[i] := 'R';

			end else begin

				inc(i);
				s[i] := 'R';
				inc(i);
				s[i] := 'L';

			end;

			inc(l);
			dec(r);
			increasing := not increasing;
		end;

		if l = r then s[n] := 'L';

		writeln(s);

	end;
end.

```
