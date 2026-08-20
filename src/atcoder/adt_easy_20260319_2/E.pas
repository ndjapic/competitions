program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
type
	TItem = record
		c, x: int32;
	end;
var
	q, i, l, r, k, mn: int32;
	tp: int8;
	s: int64;
	a: array [1 .. nn] of TItem;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);
	l := 1;
	r := 0;
	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				inc(r);
				readln(a[r].c, a[r].x);
			end;

			2: begin
				readln(k);
				s := 0;
				while k > 0 do begin
					mn := min(k, a[l].c);
					inc(s, int64(mn) * a[l].x);
					dec(k, mn);
					dec(a[l].c, mn);
					if a[l].c = 0 then inc(l);
				end;
				writeln(s);
			end;

		end;
	end;
end.
