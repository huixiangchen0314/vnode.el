EMACS ?= emacs

LOAD_PATH = -L . -L tests

SRC_FILES = \
	vnode-element.el \
	vnode-element-style.el \
	vnode-element-measure.el \
	vnode-element-layout.el \
#	vnode-vnode.el \
#	vnode-diff.el \
#	vnode.el

TEST_FILES = \
	tests/vnode-element-tests.el \
	tests/vnode-element-style-tests.el \
	tests/vnode-element-measure-tests.el \
	tests/vnode-element-layout-tests.el \
#	tests/vnode-vnode-tests.el \
#	tests/vnode-diff-tests.el


TEST_LOAD = $(addprefix -l ,$(basename $(notdir $(SRC_FILES) $(TEST_FILES))))

# 生成 .elc 的隐式规则
%.elc: %.el
	$(EMACS) -Q --batch $(LOAD_PATH) -f batch-byte-compile $<

.PHONY: test clean compile

# 编译所有源文件和测试文件
compile: $(SRC_FILES:.el=.elc) $(TEST_FILES:.el=.elc)

# 运行测试（先编译再测试，保证用的是最新代码）
test: compile
	$(EMACS) -Q --batch $(LOAD_PATH) $(TEST_LOAD) \
		-l vnode-element-controls \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f $(SRC_FILES:.el=.elc) $(TEST_FILES:.el=.elc)
