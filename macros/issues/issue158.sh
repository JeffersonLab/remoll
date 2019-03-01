#!/bin/bash
build/remoll -t 1 macros/issues/issue158_parallel_enable.mac 2>&1 > issue158_parallel_enable.log
build/remoll -t 1 macros/issues/issue158_parallel_disable.mac 2>&1 > issue158_parallel_disable.log
diff -y issue158_parallel_disable.log issue158_parallel_enable.log | less

