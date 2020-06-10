serve:
	cd site && \
	hugo server \
	--buildDrafts \
	--buildFuture \
	--disableFastRender \
	--ignoreCache \
	--theme book

production-build:
	cd site && \
	hugo --theme book && \
	rm -rf public/examples

preview-build:
	cd site && \
	hugo \
	--baseURL $(DEPLOY_PRIME_URL) \
	--buildDrafts \
	--buildFuture \
	--theme book
