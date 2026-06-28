program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	PRIME = 998244353;
var
	notc, tci: int32;
	n, m, r, c, e: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function modp2(e: int64): int64;
begin
	if e = 0 then
		result := 1
	else if odd(e) then
		result := 2 * modp2(e - 1) mod PRIME
	else
		result := sqr(modp2(e div 2)) mod PRIME;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, r, c);

		// ans := pow2(r*c-1) * pow2(r-1)^(m-c) * pow2(c-1)^(n-r)
		// ans := pow2(r*c-1 + (r-1)*(m-c) + (c-1)*(n-r))

		e := (r*c-1 + (r-1)*(m-c) + (c-1)*(n-r)) mod (PRIME - 1);

		writeln(modp2(e));

	end;
end.
