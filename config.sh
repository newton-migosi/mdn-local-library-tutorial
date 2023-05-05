# export POOL_CONFIG, WARP_CONFIG, POSTGRES_CONFIG file paths from the {CURRENT_DIR}/config/static/ directory
CURRENT_DIR=$(pwd)
export POOL_CONFIG=${CURRENT_DIR}/config/static/pool.json
export WARP_CONFIG=${CURRENT_DIR}/config/static/warp.json
export POSTGRES_CONFIG=${CURRENT_DIR}/config/static/postgres.json