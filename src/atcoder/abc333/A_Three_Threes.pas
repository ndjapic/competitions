program A_Three_Threes; {$H+}
const
    maxn = 9;
var
    n, i: int8;
    s: string;

begin
    readln(n);
    setlength(s, n);
    s[1] := chr(ord('0') + n);
    for i := 2 to n do s[i] := s[1];
    writeln(s);
end.
