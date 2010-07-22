handle SIGPWR nostop noprint pass
handle SIGXCPU nostop noprint pass
handle SIGILL nostop noprint pass

define mono_backtrace
	select-frame 0
	set $i = 0
	while ($i < $arg0)
		set $foo = mono_pmip ($pc)
		if ($foo == 0x00)
			frame
		else
			printf "#%d %p in %s\n", $i, $pc, $foo
		end
	up-silently
	set $i = $i + 1
	end
end


