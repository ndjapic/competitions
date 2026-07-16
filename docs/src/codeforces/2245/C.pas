program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i, k, x, p2: int32;
	ans: boolean;
	p: array [0 .. NN] of int32;
	seen: array [0 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		k := n xor k;

		for x := 0 to n do seen[x] := false;
		p2 := 1 shl 17;

		while p2 > n do p2 := p2 div 2;

		ans := k < 2*p2;

		while ans and (p2 > 0) and (k >= n) do begin
			if k and p2 > 0 then begin
				if p2 < n then begin
					seen[p2] := true;
					dec(k, p2);
				end else
					ans := false;
			end;
			p2 := p2 div 2;
		end;

		if ans and (k < n) then begin
			seen[k] := true;
			i := 0;

			for x := n-1 downto 0 do
				if not seen[x] then begin
					p[i] := x;
					inc(i);
				end;

			for x := 0 to n-1 do
				if seen[x] then begin
					p[i] := x;
					inc(i);
				end;

			writeln('YES');
			for i := 0 to n-2 do write(p[i], ' ');
			writeln(p[n-1]);
		end else
			writeln('NO');

	end;
end.
