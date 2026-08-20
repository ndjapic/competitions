program A_Happy_Birthday_4;
var
	x, y, z: int32;

begin
	readln(x, y, z);

	// x+d = (y+d) * z
	// x+d = yz+dz
	// x - yz = dz - d
	// x - yz = d(z-1)
	// (x - yz)/(z-1) = d

	if (x >= y*z) and ((x-y*z) mod (z-1) = 0) then
		writeln('Yes')
	else
		writeln('No');
end.
