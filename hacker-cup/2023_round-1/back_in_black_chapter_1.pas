program back_in_black_chapter_1;
uses
	math;
const
	maxn = 4000 * 1000;
var
    ntc, tci: int8;
    n, q, i, b, ans: int32;
    j: int16;
    ch: char;
    s, c: array [1 .. maxn] of int8;
    nd: array [1 .. maxn] of int16;
    divs: array [1 .. maxn] of array of int32;

procedure append_div(b, i: int32);
begin
	if length(divs[i]) = nd[i] then setlength(divs[i], nd[i] * 2);
	divs[i][nd[i]] := b;
	inc(nd[i]);
end;

begin
	for i := 1 to maxn do begin
		nd[i] := 0;
		setlength(divs[i], 2);
	end;

	for b := 1 to maxn do begin
		i := b;
		while i <= maxn do begin
			append_div(b, i);
			inc(i, b);
		end;
	end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do begin
			read(ch);
			s[i] := ord(ch) - ord('0');
			c[i] := 0;
        end;
        readln;

        readln(q);
        for i := 1 to q do begin
			readln(b);
			c[b] := c[b] xor 1;
        end;

		ans := 0;
		for i := 1 to n do begin
			for j := 0 to nd[i]-1 do
				s[i] := s[i] xor c[divs[i][j]];
			inc(ans, s[i]);
			c[i] := c[i] xor s[i];
		end;

		writeln('Case #', tci, ': ', ans);

    end;
end.
