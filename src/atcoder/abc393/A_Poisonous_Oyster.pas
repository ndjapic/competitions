program A_Poisonous_Oyster;
{$MODE DELPHI}
var
    s: string;

begin
    readln(s);

    if (s[1] = 's') and (s[6] = 's') then
        writeln(1)
    else if (s[1] = 's') and (s[6] = 'f') then
        writeln(2)
    else if (s[1] = 'f') and (s[6] = 's') then
        writeln(3)
    else if (s[1] = 'f') and (s[6] = 'f') then
        writeln(4);
end.
