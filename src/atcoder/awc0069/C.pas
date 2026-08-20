program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k, ans: int32;
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure switch(i: int32);
begin
	if i > 0 then
		s[i] := chr(ord('1') - ord(s[i]) + ord('0'));
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	ans := 0;
	ch := '1';
	for i := n downto k do begin
		if odd(ans) then begin
			switch(i);
			switch(i-k);
		end;

		if s[i] <> '1' then begin
			switch(i);
			switch(i-k);
			inc(ans);
		end;
	end;

	if odd(ans) then
		ch := '0'
	else
		ch := '1';

	i := k-1;
	while (i > 0) and (s[i] = ch) do dec(i);

	if i > 0 then ans := -1;
	writeln(ans);
end.
