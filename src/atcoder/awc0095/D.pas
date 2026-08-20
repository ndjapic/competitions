program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #mask #bitcount #disjoint #sadle_point #complete_subgraph
uses
	math;
const
	NN = 8;
	MM = 255;
var
	n, k, i, j: int8;
	m, canceled, selected, total, mn, mx: int32;
	compatible: boolean;
	l, r: array [0 .. NN] of int8;
	v: array [0 .. NN] of int32;
	nobs: array [0 .. MM] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	for i := 0 to n-1 do readln(l[i], r[i], v[i]);

	nobs[0] := 0;
	m := (1 shl n) - 1;
	mn := high(int32);

	for canceled := 0 to m do begin
		nobs[canceled] := nobs[canceled div 2] + canceled mod 2;
		if nobs[canceled] = k then begin

			mx := 0;
			for selected := 0 to m do
				if selected and canceled = 0 then begin
					total := 0;
					compatible := true;

					for i := 0 to n-1 do
						if odd(selected shr i) and compatible then begin
							inc(total, v[i]);
							for j := 0 to i-1 do
								if odd(selected shr j) and compatible then
									compatible := (r[j] <= l[i]) or (r[i] <= l[j]);
						end;

					if compatible then mx := max(mx, total);
				end;
			mn := min(mn, mx);

		end;
	end;

	writeln(mn);
end.
