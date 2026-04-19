# Problem: B_Get_Min_v1.pas

```pascal
program B_Get_Min;
uses
	math;
const
	xx = 100;
var
	q, x, i, mn, tp: int8;
	bag: array [1 .. xx] of int8;

begin
	for x := 1 to xx do bag[x] := 0;

	readln(q);
	mn := 100;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(x);
				inc(bag[x]);
				mn := min(mn, x);
			end;

			2: begin
				while bag[mn] = 0 do inc(mn);
				writeln(mn);
				dec(bag[mn]);
			end;

		end;
		readln;
	end;
end.

```
