program A_9x9.pas;
var
    a1, a2: int8;
    ch1, ch2: char;

begin
    readln(ch1, ch2, ch2);

    a1 := ord(ch1) - ord('0');
    a2 := ord(ch2) - ord('0');

    writeln(a1 * a2);
end.
