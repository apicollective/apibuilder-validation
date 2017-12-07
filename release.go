package main

import (
	"github.com/flowcommerce/tools/executor"
)

func main() {
	executor := executor.Create("apibuilder-validation")

        executor = executor.Add("git clone git@github.com:flowcommerce/misc.git")
        executor = executor.Add("cp misc/publish_branch/publish_branch.sh .")
        executor = executor.Add("rm -rf misc")
        executor = executor.Add("./publish_branch.sh")
        executor = executor.Add("rm publish_branch.sh")

	executor.Run()
}
