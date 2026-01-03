program A_Simple_Palindrome;
var
    ntc, tci, n, i, d, m, v, j: int8;
    s, vowels: string;

begin
    vowels := 'aeiou';
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        setlength(s, n);
        d := n div 5;
        m := n mod 5;
        i := 0;

        for v := 1 to m do
            for j := 1 to d+1 do begin
                inc(i);
                s[i] := vowels[v];
            end;

        for v := m+1 to 5 do
            for j := 1 to d do begin
                inc(i);
                s[i] := vowels[v];
            end;

        writeln(s);

    end;
end.
