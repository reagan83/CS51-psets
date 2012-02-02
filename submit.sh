/bin/sh

die () {
	echo "$0: error: $*" >&2
	exit 1
}

echo "CS51: Submitting Problem Set $1"

git commit -a -m"Done with ps$1.  Ready to submit." || die "commit failed"
git tag -f ps$1-submission || die "tagging failed"
git push --tags || die "pushing failed"

echo "Submission complete."
echo "You should now check that you have received a confirmation email."
