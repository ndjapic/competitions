program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
uses
	math;
const
	NN = 5001;
var
	notc, tci, n, i, ans: int32;
	loop: boolean;
	l, r, u, v, lr, rr: array [0 .. NN] of int32;
	active: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			readln(l[i], r[i], u[i], v[i]);
			active[i] := true;
		end;

		lr[0] := 0;
		rr[n+1] := 0;
		loop := true;

		while loop do begin
			loop := false;

			for i := 1 to n do begin
				lr[i] := lr[i-1] + 1;
				if active[i] and (l[i] <= lr[i]) and (lr[i] <= r[i]) then begin
					active[i] := false;
					dec(lr[i]);
					loop := true;
				end;
			end;

			for i := n downto 1 do begin
				rr[i] := rr[i+1] + 1;
				if active[i] and (u[i] <= rr[i]) and (rr[i] <= v[i]) then begin
					active[i] := false;
					dec(rr[i]);
					loop := true;
				end
			end;
		end;

		ans := 0;
		for i := 1 to n do
			if active[i] then inc(ans);

		writeln(ans);

	end;
end.
