# Problem: C_Brackets_Stack_Query.pas

```pascal
program C_Brackets_Stack_Query;
{$MODE DELPHI}
uses
	math;
const
	nn = 800 * 1000 + 1;
var
	q, n, i, j, tp: int32;
	c: char;
	h, ind: array [0 .. nn] of int32;

begin
	readln(q);
	n := 0;
	ind[0] := -1;
	h[0] := 0;
	j := 0;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(c);
				read(c);
				inc(n);

				case c of
					'(': h[n] := h[n-1] + 1;
					')': h[n] := h[n-1] - 1;
				end;

				if h[n] < 0 then begin
					inc(j);
					ind[j] := n;
				end;

			end;

			2: begin
				if ind[j] = n then dec(j);
				dec(n);
			end;

		end;
		readln;

		if (j = 0) and (h[n] = 0) then
			writeln('Yes')
		else
			writeln('No');
	end;
end.

```
