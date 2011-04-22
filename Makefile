fuse: DCFs
	./DCFs

umount:
	fusermount -u mnt

DCFs: DCFs.hs
	ghc -threaded --make DCFs
